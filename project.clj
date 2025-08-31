(defproject org.soulspace/qclojure-ibmq "0.1.0-SNAPSHOT"
  :description "Provides an IBM Quantum Backend for QClojure."
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/spec.alpha "0.3.218"]
                 [org.babashka/http-client "0.4.23"]
                 [org.soulspace/qclojure "0.12.0"]]

  :scm {:name "git" :url "https://github.com/lsolbach/qclojure-ibmq"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
