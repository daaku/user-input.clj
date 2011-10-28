(ns user-input.core
  "A library to process incoming user input, including transforming it and
  validating it, returning error messages as appropriate."
  {:author "Naitik Shah"}
  (:refer-clojure :exclude [filter])
  (:use
    [clojure.string :only [join trim blank?] :rename {trim str-trim}]
    [clojure.contrib.string :only [as-str]]))

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
       (fn [~data] [~data (do ~@body)]))))

(defmacro deftransform [name args & body]
  (let [[data & options] args]
    `(defn ~name [~@options]
       (fn [~data] [(do ~@body) {}]))))

(defn- trim-if-string [val]
  (if (string? val) (str-trim val) val))

(deftransform filter [data & keys]
  (into {} (clojure.core/filter (fn [[k v]] (some #(= % k) keys)) data)))

(deftransform trim [data]
  (into {} (map (fn [[k v]] [k (trim-if-string v)]) data)))

(defvalidator required [data & keys]
  (apply merge (map #(if (blank? (get data %)) {% "This is required."}) keys)))

(defvalidator at-least-one-of [data & keys]
  (if (every? #(blank? (get data %)) keys)
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
    (if (and (not (blank? value)) (not (is-email? value)))
      {key "Must be a valid email address."})))

(defvalidator phone [data key]
  (if (not (is-phone? (get data key))) {key "Must be a valid phone number."}))

(defvalidator password [data main confirm]
  (if (not (= (get data main) (get data confirm)))
    {confirm "Passwords do not match."}))

(defn run [fns data]
  (reduce
    (fn [[old-data old-errors] f]
      (let [[new-data new-errors] (f old-data)]
        [new-data (merge new-errors old-errors)]))
    [data {}]
    fns))
