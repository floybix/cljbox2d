(defproject org.nfrac/cljbox2d "0.5.0"
  :description "A clojure wrapper for JBox2D, for 2D physics simulation."
  :url "https://github.com/floybix/cljbox2d"
  :scm {:name "git"
        :url "https://github.com/floybix/cljbox2d"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.jbox2d/jbox2d-library "2.2.1.1"]
                 [org.slf4j/slf4j-simple "1.7.9"]
                 [org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:plugins [[lein-marginalia "0.8.0"]]}}
  :deploy-repositories [["releases" {:url "https://clojars.org/repo/"
                                     :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Felix Andrews"]
                              [:url "http://nfrac.org/felix/"]
                              [:email "felix@nfrac.org"]
                              [:timezone "+10"]]])
