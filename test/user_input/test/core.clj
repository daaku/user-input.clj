(ns user-input.test.core
  "Test user-input functionality."
  {:author "Naitik Shah"}
  (:require
    [clj-time.format :as ftime]
    [user-input.core :as user-input])
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

(deftest drop-empty
  (is (= [{:a 1} {}]
         (user-input/run [(user-input/drop-empty)] {:a 1 :b ""})))
  (is (= [{:a 1 :c ""} {}]
         (user-input/run [(user-input/drop-empty :b)] {:a 1 :b "" :c ""}))))

(deftest nil-empty
  (is (= [{:a 1 :b nil} {}]
         (user-input/run [(user-input/nil-empty)] {:a 1 :b ""})))
  (is (= [{:a 1 :b nil :c ""} {}]
         (user-input/run [(user-input/nil-empty :b)] {:a 1 :b "" :c ""}))))

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

(deftest integer
  (is (= [{:a 1} {}] (user-input/run [(user-input/integer :a)] {:a "1"})))
  (is (= [{:a 1} {}] (user-input/run [(user-input/integer :a)] {:a 1})))
  (is (= [{:a 1} {}] (user-input/run [(user-input/integer :a)] {:a 1.0})))
  (is (= [{:b 1.1} {}] (user-input/run [(user-input/integer :a)] {:b 1.1})))
  (is (= #{:a} (error-keys [(user-input/integer :a)] {:a "1.1"})))
  (is (= #{:a} (error-keys [(user-input/integer :a)] {:a "1.a"})))
  (is (= #{:a} (error-keys [(user-input/integer :a)] {:a " 1"})))
  (is (= #{:a} (error-keys [(user-input/integer :a)] {:a :b}))))

(deftest float-test
  (is (= [{:a (float 1.1)} {}]
         (user-input/run [(user-input/float :a)] {:a "1.1"})))
  (is (= [{:a (float 1.1)} {}]
         (user-input/run [(user-input/float :a)] {:a 1.1})))
  (is (= [{:a 1.0} {}] (user-input/run [(user-input/float :a)] {:a "1"})))
  (is (= #{:a} (error-keys [(user-input/float :a)] {:a "1.a"}))))

(deftest double-test
  (is (= [{:a (double 1.1)} {}]
         (user-input/run [(user-input/double :a)] {:a "1.1"})))
  (is (= [{:a (double 1.1)} {}]
         (user-input/run [(user-input/double :a)] {:a 1.1})))
  (is (= [{:a 1.0} {}] (user-input/run [(user-input/double :a)] {:a "1"})))
  (is (= #{:a} (error-keys [(user-input/double :a)] {:a "1.a"}))))

(def time-filter
  (user-input/time (ftime/formatter "yyyy-MM-dd'T'hh:mm'Z'") :time :time2))

(deftest time-test
  (is (= org.joda.time.DateTime
         (type (:time (first (time-filter
                               {:time "2011-11-21T05:51Z"} {}))))))
  (is (= #{:time} (error-keys [time-filter] {:time "1"}))))

(deftest time<
  (is (= #{:time :time2} (error-keys [time-filter
                                      (user-input/time< :time :time2)]
                                     {:time "2011-11-21T05:51Z"
                                      :time2 "2011-11-21T05:51Z"}))
      "Equal values are not accepted")
  (is (= #{:time :time2} (error-keys [time-filter
                                      (user-input/time< :time :time2)]
                                     {:time "2012-11-21T05:51Z"
                                      :time2 "2011-11-21T05:51Z"}))
      "Greater values are not accepted")
  (is (= #{} (error-keys [time-filter
                          (user-input/time< :time :time2)]
                         {:time "2010-11-21T05:51Z"
                          :time2 "2011-11-21T05:51Z"}))
      "Lower values are accepted"))

(deftest time<=
  (is (= #{} (error-keys [time-filter
                          (user-input/time<= :time :time2)]
                         {:time "2011-11-21T05:51Z"
                          :time2 "2011-11-21T05:51Z"}))
      "Equal values are accepted")
  (is (= #{:time :time2} (error-keys [time-filter
                                      (user-input/time<= :time :time2)]
                                     {:time "2012-11-21T05:51Z"
                                      :time2 "2011-11-21T05:51Z"}))
      "Greater values are not accepted")
  (is (= #{} (error-keys [time-filter
                          (user-input/time<= :time :time2)]
                         {:time "2010-11-21T05:51Z"
                          :time2 "2011-11-21T05:51Z"}))
      "Lower values are accepted"))
