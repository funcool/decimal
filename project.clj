(defproject funcool/decimal "0.1.0"
  :description "An arbitrary-precision Decimal type for ClojureScript."
  :url "https://github.com/funcool/decimal"
  :license {:name "Public Domain" :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.7.228" :scope "provided"]]
  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}
  :source-paths ["src" "assets"]
  :test-paths ["test"]
  :jar-exclusions [#"\.swp|\.swo|user.clj"]

  :codeina {:sources ["src"]
            :reader :clojurescript
            :target "doc/dist/latest/api"
            :src-uri "http://github.com/funcool/decimal/blob/master/"
            :src-uri-prefix "#L"}

  :plugins [[funcool/codeina "0.3.0"]])
