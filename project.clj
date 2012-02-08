(defproject user-input "1.0.13"
  :description "Transform, validate and prepare user input."
  :author "Naitik Shah <n@daaku.org>"
  :url "https://github.com/nshah/user-input.clj"
  :repl-init user-input.repl
  :checksum-deps true
  :exclusions [org.clojure/clojure]
  :dependencies
    [[clj-time "0.3.3"]
     [org.clojure/clojure "1.3.0"]]
  :dev-dependencies
    [[auto-reload "1.0.3"]
     [lein-marginalia "0.7.0-20111019.122151-1"]
     [org.clojure/tools.logging "0.2.3"]
     [vimclojure/server "2.3.1"]])
