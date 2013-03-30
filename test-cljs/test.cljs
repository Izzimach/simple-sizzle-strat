(ns cemerick.cljs.test.example
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testin)])
  (:require [cemerick.cljs.test :as t]))

(deftest somewhat-less-wat
  (is (= "{}[]" (+ {} []))))
