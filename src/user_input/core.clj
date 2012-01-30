(ns user-input.core
  "A library to process incoming user input, including transforming it and
   validating it, returning error messages as appropriate.

   At it's core, this library follows a simple protocol:

    (process data errors) -> [data errors]

   That is, a process takes two maps, the input data and the current errors and
   returns new versions of both. This is basic premise behind the `run`
   function which is a simple `reduce` with this contract in mind.

   With that in mind, an ordered vector of such functions forms for the \"input
   process\". For example:

    (def login-process
      [(user-input.core/filter :login :password :remember-me)
       (user-input.core/required :login :password)])

   Which can then be applied as:

    (user-input.core/run login-process {:user \"jon\"})"
  {:author "Naitik Shah"}
  (:refer-clojure :exclude [filter drop float double time])
  (:require
    [clj-time.core :as ctime]
    [clj-time.format :as ftime])
  (:use
    [clojure.string :only [join trim blank?] :rename {trim str-trim}]))

;; # The "runner"

(defn run
  "This is the basic process mentioned above. A simple reduce that allows
  functions that given some input data generates output data and errors."
  [fns data]
  (reduce (fn [[data errors] f] (f data errors)) [data {}] fns))

;; # Internal Utilities

(defn- as-str
  "From old clojure.contrib to get a Clojure aware string representation."
  [x]
  (if (instance? clojure.lang.Named x) (name x) (str x)))

(defn- join-keys
  "Joins key names using commas and a special seperator for the last element.
  This allows us to generate pretty error messages."
  [last-sep keys]
  (case (count keys)
    0 ""
    1 (as-str (first keys))
    2 (join last-sep (map as-str keys))
    (str (join ", " (map as-str (drop-last keys)))
         last-sep
         (as-str (last keys)))))

(defn- trim-if-string
  "Trim strings if possible."
  [val]
  (if (string? val) (str-trim val) val))

(defn- missing?
  "Identified values that are considered *empty* or *missing*."
  [val]
  (cond
    (nil? val) true
    (string? val) (blank? val)
    (seq? val) (empty? val)
    :else false))

;; # Utilities
;; These are simple functions that work on input values and can be used
;; standalone.

(defn is-email?
  "Check if the given input value is an email address."
  [value]
  (re-matches #"(?x)[a-z0-9!\#$%&'*+/=?^_|}~-]+
              (?:\.[a-z0-9!\#$%&'*+/=?^_|}~-]+)*
              @(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
              [a-z0-9](?:[a-z0-9-]*[a-z0-9])?" value))

(defn is-phone?
  "Check if the given input value is a phone number."
  [value]
  true)

;; # Generic Processes
;; Generic types of processes that get applied to user input:

(defmacro defvalidator
  "**Defines a validator**, which does not modify data, but may generate errors
  if the validation routine fails. A validator is a function that is given the
  input data map as the first argument along with the rest of the options to the
  validator and returns a map of errors that is merged into the existing map
  of errors."
  [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [data# errors#]
         (let [~data data#]
           [data# (merge (do ~@body) errors#)])))))

(defmacro deftransform
  "**Defines a transform**, which modifies data, but does not generate errors.
  A transform is a function that is given the input data map as the first
  argument along with the rest of the options to the transform and returns a
  new version of the data."
  [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [~data errors#] [(do ~@body) errors#]))))

(defmacro defpostprocess
  "**Defines a post processing step**, which is only applied if there were no
  errors (identical to transforms except for this condition)."
  [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [data# errors#]
         (let [~data data#]
           [(if (empty? errors#) (do ~@body) data#) errors#])))))

;; # Validators
;; Validators do not transform the data in any way. They validate the data as
;; requested and provide error messages if the validation fails.

(defvalidator required [data & keys]
  (apply merge
         (map #(if (missing? (get data %)) {% "This is required."}) keys)))

(defvalidator at-least-one-of [data & keys]
  (if (every? #(missing? (get data %)) keys)
    (zipmap keys
            (repeatedly (constantly (str "One of "
                                         (join-keys " or " keys)
                                         " is required."))))))

(defvalidator max-length [data key limit]
  (if (> (count (get data key)) limit)
    {key (str "Can't be more than " limit " characters.")}))

(defvalidator min-length [data key limit]
  (if (<= (count (get data key)) limit)
    {key (str "Must be more than " limit " characters.")}))

(defvalidator email [data key]
  (if-let [value (get data key)]
    (if (and (not (missing? value)) (not (is-email? value)))
      {key "Must be a valid email address."})))

(defvalidator phone [data key]
  (if (not (is-phone? (get data key))) {key "Must be a valid phone number."}))

(defvalidator password [data main confirm]
  (if (not (= (get data main) (get data confirm)))
    {confirm "Passwords do not match."}))

;; # Post-Processing
;; These are applied only if there are no errors. You'll typically have these
;; listed at the bottom of your process vector.

(defpostprocess drop [data & keys]
  (apply dissoc data keys))

(defpostprocess default [data & defaults]
  (merge (apply assoc {} defaults) data))

;; # Transforms
;; Transforms alter the data in some way, but do not generate any errors.

(deftransform filter [data & keys]
  (into {} (clojure.core/filter (fn [[k v]] (some #(= % k) keys)) data)))

(deftransform trim [data]
  (into {} (map (fn [[k v]] [k (trim-if-string v)]) data)))

(deftransform drop-empty [data & keys]
  (let [keys (if (seq keys) (set keys) nil)]
    (into {} (clojure.core/filter
               (fn [[k v]] (if (and keys (not (keys k)))
                             true
                             (not (missing? v))))
                 data))))

(deftransform nil-empty [data & keys]
  (let [keys (if (seq keys) (set keys) nil)]
    (into {} (map
               (fn [[k v]] (if (or (and keys (not (keys k))) (not (missing? v)))
                             [k v]
                             [k nil]))
                 data))))

;; # Validating Transforms
;; In other words, parsing something.

;; ## Number Parsing

(defmacro defnumber-parser [name parse cast thing]
  `(defn ~name [key#]
     (let [er# {key# (str "Must be a valid " ~thing ".")}]
       (fn [data# errors#]
         (if-let [val# (data# key#)]
           (try
             [(assoc data# key# (if (string? val#)
                                  (~parse val#)
                                  (~cast val#)))
              errors#]
             (catch NumberFormatException e# [data# (merge er# errors#)])
             (catch ClassCastException e# [data# (merge er# errors#)]))
           [data# errors#])))))

(defnumber-parser integer Integer/parseInt int "integer")
(defnumber-parser float Float/parseFloat clojure.core/float "number")
(defnumber-parser double Double/parseDouble clojure.core/double "number")

;; ## Time Parsing
;; This uses [clj-time.format/parse][ftime] to parse the input.
;;
;; [ftime]: https://github.com/seancorfield/clj-time

(defn- time*
  "Internal function that does the work for a single key."
  [formatter [data errors] key]
  (if-let [val (data key)]
    (try
      [(assoc data key (ftime/parse formatter val)) errors]
      (catch IllegalArgumentException e
        [data (merge {key "Must be a valid date & time."} errors)]))
    [data errors]))

(defn time
  "Parse a string using [clj-time.format/parse][ftime] using the given
  formatter. For example:

    (user-input.core/time \"yyyy-MM-dd'T'hh:mm'Z'\" :start :end)

  [ftime]: https://github.com/seancorfield/clj-time"
  [formatter & keys]
  (fn [data errors]
    (reduce (partial time* formatter) [data errors] keys)))

;; ### clj-time range validations

(defvalidator time< [data less-key more-key]
  (if-let [less (data less-key)]
    (if-let [more (data more-key)]
      (if (not (ctime/before? less more))
        {less-key (str "Must be before " (as-str more-key) ".")
         more-key (str "Must be after " (as-str less-key) ".")}))))

(defvalidator time<= [data less-key more-key]
  (if-let [less (data less-key)]
    (if-let [more (data more-key)]
      (if (ctime/after? less more)
        {less-key (str "Must be on or before " (as-str more-key) ".")
         more-key (str "Must be on or after " (as-str less-key) ".")}))))
