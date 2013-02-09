(ns simplestratserver.httpserver
  (:require [cljs.nodejs :as node]))

(def connect
  (node/require "connect"))

(def http
  (node/require "http"))

(def listenport 5000)

(defn start [& _]
  (let [connectinstance (connect)
        ;; serves files from the resources directory
        static-server (aget connect "static")]
    (-> connectinstance
        (.use (static-server "resources/public"))
        (.listen listenport))
    (println "Static server started on port" listenport)))


(set! *main-cli-fn* start)
