(ns simplestrat.httpserver
  (:use compojure.core)
  (:require [compojure.route :as route]))

(defroutes app
  (GET "/" [] "<h1>Hello World</h1>")
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))

