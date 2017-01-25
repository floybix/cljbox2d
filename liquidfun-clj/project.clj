(defproject org.nfrac/liquidfun-clj "0.1.0-SNAPSHOT"
  :description "A clojure wrapper for LiquidFun, for 2D physics simulation."
  :url "https://github.com/floybix/cljbox2d"
  :scm {:name "git"
        :url "https://github.com/floybix/cljbox2d"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :deploy-repositories [["releases" {:url "https://clojars.org/repo/"
                                     :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Felix Andrews"]
                              [:url "http://nfrac.org/felix/"]
                              [:email "felix@nfrac.org"]
                              [:timezone "+10"]]]

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha11"]
                 [org.bytedeco.javacpp-presets/liquidfun-platform "1.1.0-1.3.2-SNAPSHOT"]]

  :source-paths ["src"])
