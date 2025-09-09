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


(defn- normalize-job-id [resp]
  (or (some-> resp :body :id) (some-> resp :body :job_id) (some-> resp :body)))


(defrecord IbmQuantumBackend [state jobs]
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
    {:circuit-summary {:gate-count (get circuit :gate-count)
                       :qubits (get circuit :num-qubits)}
     :options options
     :estimated-cost nil
     :notes "IBM public API does not expose cost estimation"})

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
      {:error "unknown-batch" :batch-id batch-id})))


(defn create-ibm-quantum-backend
  "Factory for IBM Quantum backend adapter.

  Options:
  * api-token (string) optional initial token
  Returns backend record implementing QClojure protocols.
  "
  ([] (create-ibm-quantum-backend nil))
  ([api-token]
   (->IbmQuantumBackend (atom {:api-token api-token}) (atom {}))))
