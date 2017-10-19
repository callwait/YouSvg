(defproject svg "0.1.4-SNAPSHOT"
  :description "YouTrack svg builder"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :main svg.core
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.5.1"]
                 [ring/ring-core "1.6.2"]
                 [ring/ring-jetty-adapter "1.6.2"]
                 [ring/ring-defaults "0.2.1"]
                 [clj-http "3.7.0"]]
  :plugins [[lein-ring "0.12.1"]]
  :ring {:handler svg.core/app-routes}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})

