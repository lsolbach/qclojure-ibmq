(ns org.soulspace.qclojure.adapter.backend.ibmq
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [babashka.http-client :as http]
            [martian.core :as martian]
            [martian.babashka.http-client :as martian-http]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.application.hardware-optimization :as hwopt]
            [org.soulspace.qclojure.application.format.qasm3 :as qasm3]))


; https://quantum.cloud.ibm.com/api/openapi.json
(def ibmq (martian-http/bootstrap-openapi "https://quantum.cloud.ibm.com/api/openapi.json"))


(comment
  ;; Example how to use the ibmq martian client
  (martian/explore ibmq)
  (martian/explore ibmq :list-backends)
  (martian/explore ibmq :get-backend-defaults)
  (martian/explore ibmq :get-backend-properties)
  (martian/explore ibmq :get-backend-configuration)

  (martian/url-for ibmq :list-backends)
  (martian/request-for ibmq :list-backends)
  (martian/response-for ibmq :list-backends) ; Status 401 - Unauthorized - need to provide an API token

  ;(martian/url-for ibmq :get-backend-properties)

  ;
  )

