(ns user-input.test.core
  "Test user-input functionality."
  {:author "Naitik Shah"}
  (:require [user-input.core :as user-input])
  (:use [clojure.test :only [deftest testing is]]))

(deftest is-email?
  (dorun (map #(is (= % (user-input/is-email? %)) (str % " should be valid."))
              ["a@a.com" "a.b@com.com"])))

(deftest invalid-is-email?
  (dorun (map #(is (not (user-input/is-email? %)) (str % " should be invalid."))
              ["" "a" "a@" "a@a" "a@a."])))
