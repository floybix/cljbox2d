(defproject org.nfrac/liquidfun-clj.testbed "0.1.0-SNAPSHOT"
  :description "Visual demos of LiquidFun."
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
                 [org.nfrac/liquidfun-clj "0.1.0-SNAPSHOT"]
                 [quil "2.5.0"]]

  :source-paths ["src"])
