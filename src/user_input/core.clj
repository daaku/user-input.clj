(ns user-input.core
  "A library to process incoming user input, including transforming it and
  validating it, returning error messages as appropriate."
  {:author "Naitik Shah"}
  (:refer-clojure :exclude [filter drop float double])
  (:use
    [clojure.string :only [join trim blank?] :rename {trim str-trim}]))

(defn as-str [x] (if (instance? clojure.lang.Named x) (name x) (str x)))

(defn- join-keys [last-sep keys]
  (case (count keys)
    0 ""
    1 (first keys)
    2 (join last-sep (map as-str keys))
    (str (join ", " (map as-str (drop-last keys)))
         last-sep
         (as-str (last keys)))))

(defn is-email? [value]
  (re-matches #"(?x)[a-z0-9!\#$%&'*+/=?^_|}~-]+
              (?:\.[a-z0-9!\#$%&'*+/=?^_|}~-]+)*
              @(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
              [a-z0-9](?:[a-z0-9-]*[a-z0-9])?" value))

(defn is-phone? [value]
  true)

(defmacro defvalidator [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [data# errors#]
         (let [~data data#]
           [data# (merge (do ~@body) errors#)])))))

(defmacro deftransform [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [~data errors#] [(do ~@body) errors#]))))

(defmacro defpostprocess [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [data# errors#]
         (let [~data data#]
           [(if (empty? errors#) (do ~@body) data#) errors#])))))

(defn- trim-if-string [val]
  (if (string? val) (str-trim val) val))

(defn missing? [val]
  (cond
    (nil? val) true
    (string? val) (blank? val)
    (seq? val) (empty? val)
    :else false))

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

(defpostprocess drop [data & keys]
  (apply dissoc data keys))

(defpostprocess default [data & defaults]
  (merge (apply assoc {} defaults) data))

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

(defn run [fns data]
  (reduce (fn [[data errors] f] (f data errors)) [data {}] fns))
