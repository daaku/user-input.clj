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

(def fixed-error-assoc {:answer 42})
(user-input/defvalidator fixed-error [data] fixed-error-assoc)
(user-input/defvalidator no-errors [data] {})

(deftest defvalidators
  (let [input {:foo 42 :answer "bar"}]
    (is (= [input {}] (user-input/run [(no-errors)] input))
        "no change to input and no errors")
    (is (= [input fixed-error-assoc] (user-input/run [(fixed-error)] input))
        "no change to input and fixed errors")))

(def fixed-transform-assoc {:answer 42})
(user-input/deftransform fixed-transform [data] fixed-transform-assoc)
(user-input/deftransform no-transforms [data] data)

(deftest deftransforms
  (let [input {:foo 42 :answer "bar"}]
    (is (= [input {}] (user-input/run [(no-transforms)] input))
        "no change to input and no errors")
    (is (= [fixed-transform-assoc {}]
           (user-input/run [(fixed-transform)] input))
        "fixed new input and no errors")))

(deftest filters
  (let [base {:a 1 :b 2}
        junk (merge base {:c 3 :d 4})]
    (is (= [base {}]
           (user-input/run [(apply user-input/filter (keys base))] junk)))))

(deftest trims
  (is (= [{:a "1" :b "2"} {}]
         (user-input/run [(user-input/trim)] {:a " 1" :b "\n 2\n\n "}))
      "trims whitespace including newlines")
  (is (= [{:a "1\n2"} {}] (user-input/run [(user-input/trim)] {:a " 1\n2\n"}))))
