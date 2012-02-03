(ns user-input.repl
  "repl helpers"
  {:author "Naitik Shah"}
  (:require
    [auto-reload.core]
    [clojure.string]
    [clojure.tools.logging]
    [user-input.core]))

(auto-reload.core/auto-reload ["src"])
