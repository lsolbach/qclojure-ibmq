(defproject org.soulspace/qclojure-ibmq "0.1.0-SNAPSHOT"
  :description "Provides an IBM Quantum Backend for QClojure."
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [org.clojure/data.json "2.5.1"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [org.babashka/http-client "0.4.23"]
                 [com.github.oliyh/martian "0.2.0"]
                 [com.github.oliyh/martian-babashka-http-client "0.2.0"]
                 [org.soulspace/qclojure "0.17.0"]]

  :scm {:name "git" :url "https://github.com/lsolbach/qclojure-ibmq"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
