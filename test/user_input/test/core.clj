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
(user-input/defvalidator uses-destructuring [{:keys [a] :as data}] {})

(deftest defvalidators
  (let [input {:foo 42 :answer "bar"}]
    (is (= [input {}] (user-input/run [(no-errors)] input))
        "no change to input and no errors")
    (is (= [input fixed-error-assoc] (user-input/run [(fixed-error)] input))
        "no change to input and fixed errors")
    (is (= [input {}] (user-input/run [(uses-destructuring)] input))
        "no change to input and no errors")))

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

(defn- error-keys [fns data]
  (set (keys (get (user-input/run fns data) 1))))

(deftest requires
  (is (= #{:a} (error-keys [(user-input/required :a)] {})) "a error")
  (is (= #{} (error-keys [(user-input/required :a)] {:a "1"})) "no error")
  (is (= #{} (error-keys [(user-input/required :a)] {:a 1})) "int value"))

(deftest at-least-one-of
  (is (= #{:a :b} (error-keys [(user-input/at-least-one-of :a :b)] {})))
  (is (= #{} (error-keys [(user-input/at-least-one-of :a :b)] {:a "1"})))
  (is (= #{} (error-keys [(user-input/at-least-one-of :a :b)] {:a 1}))))

(deftest drop-unless-error
  (is (= [{:a 1} {}] (user-input/run [(user-input/drop :b)] {:a 1 :b 2})))
  (is (= {:a 1 :b 2} (first (user-input/run [(user-input/required :c)
                                             (user-input/drop :b)]
                                            {:a 1 :b 2})))))

(deftest default
  (is (= [{:a 1 :b 2} {}] (user-input/run [(user-input/default :b 2)] {:a 1})))
  (is (= {:a 1} (first (user-input/run [(user-input/required :c)
                                        (user-input/default :b 2)]
                                       {:a 1})))))
