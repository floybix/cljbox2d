(defproject org.nfrac/cljbox2d.testbed "0.5.0"
  :description "Visual testbed for cljbox2d worlds - basic drawing and interaction."
  :url "https://github.com/floybix/cljbox2d"
  :scm {:name "git"
        :url "https://github.com/floybix/cljbox2d"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.nfrac/cljbox2d "0.5.0"]
                 [quil "2.2.4"]
                 [org.clojure/clojure "1.6.0"]]
  :deploy-repositories [["releases" {:url "https://clojars.org/repo/"
                                     :creds :gpg}]])
