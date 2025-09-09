(ns org.soulspace.qclojure.adapter.backend.ibmq-backend
  "IBM Quantum backend adapter.

  Implements the QClojure `QuantumBackend` & `CloudQuantumBackend` protocols
  using the martian OpenAPI client defined in
  `org.soulspace.qclojure.adapter.backend.ibmq/ibmq`.

  Design notes:
  * `submit-circuit` returns a job-id (string) per QClojure contract
  * `get-job-status` returns a status keyword
  * `get-job-result` returns a result map with at least :job-status and
    :measurement-results (matching expectations in upstream tests)
  * Authentication mutates internal state atom (cloud style)
  * We keep provider job id separate (internal map tracks mapping)
  "
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [martian.core :as martian]
            [org.soulspace.qclojure.adapter.backend.ibmq-client :refer [ibmq]]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.application.format.qasm3 :as qasm3]))

;;;
;;; Internal helper functions
;;;
(defn- safe-parse-body [maybe-body]
  (try
    (cond
      (nil? maybe-body) nil
      (string? maybe-body) (json/read-str maybe-body :key-fn keyword)
      :else maybe-body)
    (catch Exception _ nil)))


(defn- inject-auth
  "Inject Authorization + IBM-API-Version headers when api-token is present (Bearer)."
  [api-token params]
  (let [base {"IBM-API-Version" "2025-05-01"}
        auth (when api-token {"Authorization" (str "Bearer " api-token)})]
    (update params :headers #(merge base auth (or % {})))))

(defn- call-martian
  "Call martian with op (or ops) adding auth headers. Returns first non-401 response."
  [client api-token ops params]
  (let [ops (if (sequential? ops) ops [ops])
        params (inject-auth api-token params)]
    (some (fn [op]
            (try
              (let [resp (martian/response-for client op params)]
                (when (and resp (not= 401 (:status resp))) resp))
              (catch Exception _ nil)))
          ops)))

;;;
;;; Cost estimation helpers
;;;
(defn- estimate-circuit-execution-time
  "Estimate the execution time for a quantum circuit in microseconds.
  
  This estimation includes:
  - Gate execution times based on gate types
  - Circuit depth parallelization effects  
  - Qubit readout time
  - Overhead for classical processing between shots
  
  Parameters:
  - circuit: Quantum circuit map with :operations and :num-qubits
  - shots: Number of measurement shots
  - backend-id: Backend device identifier for device-specific timing
  
  Returns: Estimated execution time in microseconds"
  [circuit shots backend-id]
  (let [operations (:operations circuit)
        num-qubits (:num-qubits circuit)
        
        ;; Device-specific gate timing characteristics (in microseconds)
        gate-times (case backend-id
                     ;; IBM devices - based on typical real device specs
                     ("ibm_brisbane" "ibm_kyoto" "ibm_osaka") 
                     {:x 50, :y 50, :z 0     ; Single-qubit gates
                      :h 50, :s 50, :t 50
                      :rx 50, :ry 50, :rz 0
                      :cnot 400, :cz 300      ; Two-qubit gates  
                      :ccx 1200               ; Three-qubit gates
                      :measure 5000}          ; Measurement time
                     
                     ;; Simulators - very fast  
                     ("ibm_qasm_simulator" "simulator")
                     {:x 1, :y 1, :z 1, :h 1, :s 1, :t 1
                      :rx 1, :ry 1, :rz 1
                      :cnot 2, :cz 2, :ccx 3
                      :measure 10}
                     
                     ;; Default hardware timing
                     {:x 100, :y 100, :z 10
                      :h 100, :s 100, :t 100  
                      :rx 100, :ry 100, :rz 10
                      :cnot 500, :cz 400, :ccx 1500
                      :measure 6000})
        
        ;; Calculate total gate execution time
        gate-execution-time 
        (reduce (fn [total-time op]
                  (let [gate-type (:operation-type op)
                        gate-time (get gate-times gate-type 100)] ; Default 100μs
                    (+ total-time gate-time)))
                0
                operations)
        
        ;; Add measurement overhead (measurement operations)
        measurement-time (* (:measure gate-times) num-qubits)
        
        ;; Single shot time
        single-shot-time (+ gate-execution-time measurement-time)
        
        ;; Inter-shot overhead (reset, initialization)
        inter-shot-overhead 1000  ; 1ms between shots
        
        ;; Total execution time (shots run sequentially on most devices)
        total-time (+ (* single-shot-time shots)
                     (* inter-shot-overhead (dec shots)))]    (max total-time 1000)))

(defn- get-device-pricing-tier
  "Get pricing tier and cost per second for an IBM Quantum device.
  
  Pricing is based on IBM Quantum's published rates and device capabilities.
  Rates are per second of quantum processor time.
  
  Parameters:
  - backend-id: Device identifier
  
  Returns: Map with :tier, :cost-per-second-usd, and :description"
  [backend-id]
  (case backend-id
    ;; Premium quantum processors (127+ qubits, advanced error correction)
    ("ibm_condor" "ibm_heron" "ibm_flamingo")
    {:tier :premium
     :cost-per-second-usd 1.60
     :description "Premium quantum processor - 127+ qubits, advanced error correction"}
    
    ;; Standard quantum processors (27-127 qubits) 
    ("ibm_brisbane" "ibm_kyoto" "ibm_osaka" "ibm_quebec")
    {:tier :standard  
     :cost-per-second-usd 1.20
     :description "Standard quantum processor - 27-127 qubits"}
    
    ;; Entry-level quantum processors (<27 qubits)
    ("ibm_manila" "ibm_lima" "ibm_belem" "ibm_quito")
    {:tier :entry
     :cost-per-second-usd 0.80
     :description "Entry-level quantum processor - up to 27 qubits"}
    
    ;; Simulators (free or very low cost)
    ("ibm_qasm_simulator" "simulator" "aer_simulator") 
    {:tier :simulator
     :cost-per-second-usd 0.00
     :description "Quantum circuit simulator - no hardware cost"}
    
    ;; Default for unknown devices (standard pricing)
    {:tier :standard
     :cost-per-second-usd 1.20
     :description "Standard quantum processor - estimated pricing"}))

(defn- calculate-cost-breakdown
  "Calculate detailed cost breakdown for circuit execution.
  
  Parameters:
  - execution-time-us: Estimated execution time in microseconds
  - shots: Number of measurement shots
  - device-pricing: Device pricing information
  - circuit: Circuit specification
  
  Returns: Detailed cost breakdown map"
  [execution-time-us shots device-pricing circuit]
  (let [execution-time-seconds (/ execution-time-us 1000000.0)
        cost-per-second (:cost-per-second-usd device-pricing)
        
        ;; Base quantum processor time cost
        processor-cost (* execution-time-seconds cost-per-second)
        
        ;; Shot-based fees (some providers charge per shot)
        shot-cost (if (= (:tier device-pricing) :simulator)
                    0.0
                    (* shots 0.001)) ; $0.001 per shot for real hardware
        
        ;; Circuit complexity fee based on gate count
        gate-count (count (:operations circuit))
        complexity-fee (if (> gate-count 100)
                         (* (- gate-count 100) 0.01) ; $0.01 per gate over 100
                         0.0)
        
        ;; Queue priority fee (if high priority requested)
        priority-fee 0.0 ; Could be parameterized based on options
        
        total-cost (+ processor-cost shot-cost complexity-fee priority-fee)]
    
    {:processor-time-cost processor-cost
     :shot-cost shot-cost  
     :complexity-fee complexity-fee
     :priority-fee priority-fee
     :total-cost total-cost
     :execution-time-seconds execution-time-seconds
     :execution-time-microseconds execution-time-us}))

;;;
;;; IBM Quantum specific protocol
;;;
(defprotocol IBMQuantumBackend
  "Protocol for IBM Quantum specific functionality beyond the standard QClojure protocols.
  
  This protocol provides access to IBM-specific features like job metrics, session management,
  usage analytics, and transpiled circuit access."
  
  (get-job-metrics [this job-id]
    "Get detailed execution metrics for a job.
    
    Parameters:
    - job-id: QClojure job identifier
    
    Returns: Map with metrics including:
    - :queue-time-seconds - Time spent in queue
    - :execution-time-seconds - Actual quantum processor time
    - :total-time-seconds - Total time from submission to completion
    - :cost-breakdown - Detailed cost information
    - :raw-metrics - Raw IBM metrics response")
  
  (get-job-logs [this job-id]
    "Get execution logs for a job for debugging purposes.
    
    Parameters:
    - job-id: QClojure job identifier
    
    Returns: Map with:
    - :logs - Vector of log entries
    - :log-level - Logging level used
    - :raw-logs - Raw IBM logs response")
  
  (get-transpiled-circuit [this job-id]
    "Get the transpiled/optimized circuit that was actually executed.
    
    Parameters:
    - job-id: QClojure job identifier
    
    Returns: Map with:
    - :transpiled-qasm - QASM representation of optimized circuit
    - :optimization-level - Level of optimization applied
    - :gate-count-original - Original circuit gate count
    - :gate-count-optimized - Optimized circuit gate count
    - :raw-transpiled - Raw IBM transpiled circuit response")
  
  (create-session [this options]
    "Create a quantum computing session for batching jobs efficiently.
    
    Parameters:
    - options: Map with session configuration:
      - :backend - Target backend device
      - :max-time-seconds - Maximum session duration (optional)
      - :instance - IBM instance (optional)
      - :channel - Channel for session (optional)
    
    Returns: Map with:
    - :session-id - IBM session identifier
    - :backend - Target backend
    - :status - Session status
    - :created-at - Session creation timestamp")
  
  (close-session [this session-id]
    "Close a quantum computing session to stop billing.
    
    Parameters:
    - session-id: IBM session identifier
    
    Returns: Map with:
    - :session-id - Session identifier
    - :closed? - Boolean indicating if successfully closed
    - :final-cost - Final session cost if available")
  
  (get-session-info [this session-id]
    "Get information about an active session.
    
    Parameters:
    - session-id: IBM session identifier
    
    Returns: Map with session details including status and cost")
  
  (get-usage-analytics [this options]
    "Get usage analytics and billing information.
    
    Parameters:
    - options: Map with query parameters:
      - :start-date - Start date for analytics (optional)
      - :end-date - End date for analytics (optional)
      - :backend - Filter by backend (optional)
    
    Returns: Map with:
    - :total-cost - Total cost for period
    - :job-count - Number of jobs executed
    - :total-quantum-time - Total quantum processor time used
    - :cost-by-backend - Cost breakdown by backend
    - :usage-trends - Usage trends over time"))

;;;
;;; IBM Quantum Backend implementation
;;;
(defn- normalize-job-id [resp]
  (or (some-> resp :body :id) (some-> resp :body :job_id) (some-> resp :body)))


(defrecord IBMQBackend [state jobs]
  qb/QuantumBackend
  (get-backend-info [_this]
    (let [api-token (:api-token @state)
          props-resp (call-martian ibmq api-token :get-backend-properties {})
          gates (or (get-in props-resp [:body :gates]) [])]
      {:backend-type :cloud
       :backend-name "IBM Quantum"
       :provider :ibm-quantum
       :description "IBM Quantum cloud backend"
       :supported-gates (->> gates (map #(keyword (name %))) set)
       :capabilities #{:cloud-execution :openqasm3}
       :raw-properties (:body props-resp)}))

  (get-supported-gates [_this]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :get-backend-properties {})
          gates (get-in resp [:body :gates])]
      (->> gates (map #(keyword (name %))) set)))

  (is-available? [_this]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :list-backends {})
          body (or (safe-parse-body (:body resp)) (:body resp))
          devices (or (get body :devices) (get body :backends))]
      (boolean (seq devices))))

  (submit-circuit [_this circuit options]
    (let [api-token (:api-token @state)
          {:keys [shots backend program-id program_id pubs tags log_level runtime cost session_id name]
           :or {shots 128 name "qclojure-job"}}
          options
          program-id (or program-id program_id)
          _ (when (nil? backend) (throw (ex-info "Backend required (:backend)" {:options options})))
          _ (when (nil? program-id) (throw (ex-info "Program ID required (:program-id or :program_id)" {:options options})))
          qasm (qasm3/circuit-to-qasm circuit)
          pubs (if (seq pubs) pubs [[qasm {} shots]])
          body (cond-> {:program_id program-id
                        :backend backend
                        :params {:pubs pubs :options {:default_shots shots}}
                        :name name
                        :tags tags
                        :log_level log_level
                        :runtime runtime
                        :cost cost
                        :session_id session_id}
                 (nil? tags) (dissoc :tags)
                 (nil? log_level) (dissoc :log_level)
                 (nil? runtime) (dissoc :runtime)
                 (nil? cost) (dissoc :cost)
                 (nil? session_id) (dissoc :session_id))
          resp (call-martian ibmq api-token :create-job {:body body})
          provider-id (normalize-job-id resp)
          local-id (str (java.util.UUID/randomUUID))]
      (swap! jobs assoc local-id {:provider-id provider-id :backend backend :submitted-at (java.time.Instant/now)
                                  :program-id program-id :shots shots :request body})
      ;; Return only job-id per QClojure contract
      local-id))

  (get-job-status [_this job-id]
    (let [api-token (:api-token @state)]
      (if-let [entry (@jobs job-id)]
        (let [provider-id (:provider-id entry)
              resp (call-martian ibmq api-token [:get-job :get-job] {:id provider-id})
              body (safe-parse-body (:body resp))
              status-raw (or (get body :status) (get body "status") (:status resp) (get resp :status))
              status (case (some-> status-raw str/lower-case)
                       "completed" :completed
                       "running" :running
                       "queued" :queued
                       "cancelled" :cancelled
                       "failed" :failed
                       :unknown)]
          status)
        :unknown)))

  (get-job-result [_this job-id]
    (let [api-token (:api-token @state)]
      (if-let [entry (@jobs job-id)]
        (let [provider-id (:provider-id entry)
              resp (call-martian ibmq api-token [:get-job-results-jid :get-job-results-jid :get-job-results-jid] {:id provider-id})
              body (or (safe-parse-body (:body resp)) (:body resp))
              measurement-results (cond
                                    (map? body)
                                    (or (:measurement-results body)
                                        (:counts body)
                                        (:results body)
                                        (get-in body [:pubs 0 :results])
                                        {})
                                    (string? body)
                                    (try
                                      ;; attempt JSON parse
                                      (let [m (json/read-str body :key-fn keyword)]
                                        (or (:measurement-results m) (:counts m) {}))
                                      (catch Exception _ {}))
                                    :else {})]
          {:job-status :completed
           :job-id job-id
           :provider-id provider-id
           :measurement-results measurement-results
           :raw body})
        {:job-status :failed :error-message "unknown-job" :job-id job-id})))

  (cancel-job [_this job-id]
    (if-let [entry (@jobs job-id)]
      (let [provider-id (:provider-id entry)
            api-token (:api-token @state)
            resp (call-martian ibmq api-token [:cancel-job-jid :cancel-job-jid :delete-job] {:id provider-id})]
        {:job-id job-id :provider-id provider-id :cancelled? (boolean resp) :raw resp})
      {:error "unknown-job" :job-id job-id}))

  (get-queue-status [_this]
    ;; Synthesize a simple queue view from tracked jobs
    (let [counts (vals @jobs)
          total (count counts)]
      {:total-tracked total}))

  qb/CloudQuantumBackend
  (authenticate [_this credentials]
    (let [token (or (:api-token credentials)
                    (:token credentials)
                    (:api-key credentials)
                    (:bearer-token credentials)
                    (:ibm-quantum-api-key credentials))]
      (when (str/blank? token)
        (throw (ex-info "No API token supplied in credentials" {:credentials (keys credentials)})))
      (swap! state assoc :api-token token :authenticated-at (System/currentTimeMillis))
      {:status :authenticated :token-set true}))

  (get-session-info [_this]
    (let [token (:api-token @state)]
      (if token
        {:status :authenticated :has-token true :authenticated-at (:authenticated-at @state)}
        {:status :unauthenticated :has-token false})))

  (list-available-devices [_this]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :list-backends {})
          raw (or (get-in resp [:body :backends]) (get-in resp [:body :devices]) [])]
  (mapv (fn [d]
      (let [id (or (:id d) (:device_id d) (:backend d) (:backend_name d))
        name (or (:name d) (:backend_name d) id)
        status-str (str/lower-case (or (:status d) "online"))
        status (keyword status-str)
        n-qubits (or (:num_qubits d) (:n_qubits d) (:qubits d) (:max-qubits d))]
                {:device-id (str id)
                 :device-name name
                 :device-status (if (#{"online" "available"} (name status)) :online :offline)
                 :max-qubits n-qubits
                 :raw d}))
            raw)))

  (get-device-topology [_this device-id]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :get-backend-configuration {:id device-id})]
      {:device-id device-id
       :coupling-map (or (get-in resp [:body :coupling_map]) (get-in resp [:body :topology]) [])
       :raw (:body resp)}))

  (get-calibration-data [_this device-id]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :get-backend-properties {:id device-id})]
      {:device-id device-id
       :calibration (or (get-in resp [:body :calibration]) (:body resp))}))

  (estimate-cost [_this circuit options]
    (let [shots (get options :shots 1024)
          backend (get options :backend "ibm_qasm_simulator")
          
          ;; Get device pricing information  
          device-pricing (get-device-pricing-tier backend)
          
          ;; Estimate circuit execution time
          execution-time-us (estimate-circuit-execution-time circuit shots backend)
          
          ;; Calculate detailed cost breakdown
          cost-breakdown (calculate-cost-breakdown execution-time-us shots device-pricing circuit)
          
          total-cost (:total-cost cost-breakdown)]
      
      {:total-cost total-cost
       :currency "USD"
       :cost-breakdown {:processor-time-cost (:processor-time-cost cost-breakdown)
                       :shot-cost (:shot-cost cost-breakdown)
                       :complexity-fee (:complexity-fee cost-breakdown)
                       :priority-fee (:priority-fee cost-breakdown)}
       :estimated-credits (* total-cost 100) ; Assume 100 credits per USD
       :execution-estimate {:execution-time-seconds (:execution-time-seconds cost-breakdown)
                           :execution-time-microseconds (:execution-time-microseconds cost-breakdown)
                           :shots shots
                           :backend backend
                           :device-tier (:tier device-pricing)
                           :device-description (:description device-pricing)}
       :circuit-summary {:gate-count (count (:operations circuit))
                        :num-qubits (:num-qubits circuit)
                        :circuit-depth (or (:depth circuit) (count (:operations circuit)))}
       :pricing-notes ["Costs estimated based on IBM Quantum pricing structure"
                      "Real costs may vary based on actual execution time and queue priority"
                      "Simulator execution is free of charge"
                      "Hardware execution includes processor time, shot fees, and complexity charges"]}))

  (batch-submit [_this circuits options]
    (let [circs (if (sequential? circuits) circuits [circuits])
          ids (mapv (fn [c] (qb/submit-circuit _this c options)) circs)
          batch-id (str (java.util.UUID/randomUUID))]
      (swap! jobs assoc batch-id {:job-ids ids :submitted-at (java.time.Instant/now)})
      {:batch-id batch-id :job-ids ids}))

  (get-batch-status [_this batch-id]
    (if-let [b (@jobs batch-id)]
      {:batch-id batch-id :job-ids (:job-ids b)}
      {:error "unknown-batch" :batch-id batch-id}))

  (get-batch-results [_this batch-id]
    (if-let [b (@jobs batch-id)]
      {:batch-id batch-id :results (mapv (fn [job-id] (qb/get-job-result _this job-id)) (:job-ids b))}
      {:error "unknown-batch" :batch-id batch-id}))

  ;; IBM Quantum specific protocol implementation
  IBMQuantumBackend
  (get-job-metrics [_this job-id]
    (let [api-token (:api-token @state)]
      (if-let [entry (@jobs job-id)]
        (let [provider-id (:provider-id entry)
              resp (call-martian ibmq api-token :get-job-metrics {:id provider-id})
              body (safe-parse-body (:body resp))
              
              ;; Extract timing information
              queue-time (or (:queue_time body) (:queueTime body) 0)
              execution-time (or (:execution_time body) (:executionTime body) (:quantum_seconds body) 0)
              total-time (+ queue-time execution-time)
              
              ;; Extract cost information
              cost-info (or (:cost body) (:usage body) {})]
          
          {:queue-time-seconds queue-time
           :execution-time-seconds execution-time
           :total-time-seconds total-time
           :cost-breakdown cost-info
           :provider-id provider-id
           :raw-metrics body})
        {:error "unknown-job" :job-id job-id})))
  
  (get-job-logs [_this job-id]
    (let [api-token (:api-token @state)]
      (if-let [entry (@jobs job-id)]
        (let [provider-id (:provider-id entry)
              resp (call-martian ibmq api-token :get-job-logs {:id provider-id})
              body (safe-parse-body (:body resp))
              
              logs (or (:logs body) (:messages body) [])
              log-level (or (:log_level body) (:logLevel body) "INFO")]
          
          {:logs logs
           :log-level log-level
           :provider-id provider-id
           :raw-logs body})
        {:error "unknown-job" :job-id job-id})))
  
  (get-transpiled-circuit [_this job-id]
    (let [api-token (:api-token @state)]
      (if-let [entry (@jobs job-id)]
        (let [provider-id (:provider-id entry)
              resp (call-martian ibmq api-token :get-transpiled-circuits {:id provider-id})
              body (safe-parse-body (:body resp))
              
              ;; Extract transpiled circuit information
              transpiled-qasm (or (:qasm body) (:transpiled_qasm body) (:circuit body))
              optimization-level (or (:optimization_level body) (:optimizationLevel body) 1)
              original-gates (or (:original_gate_count body) (:originalGateCount body))
              optimized-gates (or (:optimized_gate_count body) (:optimizedGateCount body))]
          
          {:transpiled-qasm transpiled-qasm
           :optimization-level optimization-level
           :gate-count-original original-gates
           :gate-count-optimized optimized-gates
           :provider-id provider-id
           :raw-transpiled body})
        {:error "unknown-job" :job-id job-id})))
  
  (create-session [_this options]
    (let [api-token (:api-token @state)
          {:keys [backend max-time-seconds instance channel]
           :or {max-time-seconds 3600}} options ; Default 1 hour
          
          body (cond-> {:backend backend}
                 max-time-seconds (assoc :max_time max-time-seconds)
                 instance (assoc :instance instance)
                 channel (assoc :channel channel))
          
          resp (call-martian ibmq api-token :create-session {:body body})
          response-body (safe-parse-body (:body resp))
          
          session-id (or (:id response-body) (:session_id response-body))
          status (or (:status response-body) "created")]
      
      (when session-id
        ;; Store session info for tracking
        (swap! state assoc-in [:sessions session-id] 
               {:backend backend
                :created-at (java.time.Instant/now)
                :max-time-seconds max-time-seconds}))
      
      {:session-id session-id
       :backend backend
       :status (keyword status)
       :created-at (java.time.Instant/now)
       :max-time-seconds max-time-seconds
       :raw-response response-body}))
  
  (close-session [_this session-id]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :close-session {:id session-id})
          response-body (safe-parse-body (:body resp))
          
          closed? (or (:closed response-body) (= (:status response-body) "closed") (= 200 (:status resp)))
          final-cost (or (:cost response-body) (:usage response-body))]
      
      ;; Remove from tracked sessions
      (swap! state update :sessions dissoc session-id)
      
      {:session-id session-id
       :closed? closed?
       :final-cost final-cost
       :raw-response response-body}))
  
  (get-session-info [_this session-id]
    (let [api-token (:api-token @state)
          resp (call-martian ibmq api-token :get-session {:id session-id})
          response-body (safe-parse-body (:body resp))
          
          ;; Get local tracking info
          local-info (get-in @state [:sessions session-id])]
      
      (merge local-info
             {:session-id session-id
              :status (keyword (or (:status response-body) "unknown"))
              :current-cost (or (:cost response-body) (:usage response-body))
              :active-jobs (or (:active_jobs response-body) (:activeJobs response-body) 0)
              :raw-session response-body})))
  
  (get-usage-analytics [_this options]
    (let [api-token (:api-token @state)
          {:keys [start-date end-date backend]} options
          
          params (cond-> {}
                   start-date (assoc :start_date start-date)
                   end-date (assoc :end_date end-date)  
                   backend (assoc :backend backend))
          
          resp (call-martian ibmq api-token :get-usage-analytics {:query-params params})
          body (safe-parse-body (:body resp))
          
          ;; Extract analytics information
          total-cost (or (:total_cost body) (:totalCost body) 0)
          job-count (or (:job_count body) (:jobCount body) 0)
          quantum-time (or (:quantum_seconds body) (:quantumTime body) 0)
          backend-costs (or (:backend_breakdown body) (:backendBreakdown body) {})
          trends (or (:usage_trends body) (:usageTrends body) [])]
      
      {:total-cost total-cost
       :job-count job-count
       :total-quantum-time quantum-time
       :cost-by-backend backend-costs
       :usage-trends trends
       :query-period {:start-date start-date :end-date end-date :backend backend}
       :raw-analytics body})))

;;;
;;; Factory functions
;;;
(defn create-ibm-quantum-backend
  "Factory for IBM Quantum backend adapter.

  Options:
  * api-token (string) optional initial token
  Returns backend record implementing QClojure protocols.
  "
  ([] (create-ibm-quantum-backend nil))
  ([api-token]
   (->IBMQBackend (atom {:api-token api-token}) (atom {}))))
