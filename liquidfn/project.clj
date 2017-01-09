(defproject org.nfrac/liquidfn "0.1.0-SNAPSHOT"
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
                 [org.clojure/clojurescript "1.9.229"]
                 [cljsjs/liquidfun "1.1.0-0"]
                 [org.bytedeco.javacpp-presets/liquidfun-platform "1.1.0-1.3.2-SNAPSHOT"]
                 [euclidean "0.2.0"]]

  :plugins [[lein-cljsbuild "1.1.4" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]

                :compiler {:main org.nfrac.liquidfn.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/liquidfn.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}
               ;; This next build is an compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once min
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/liquidfn.js"
                           :main org.nfrac.liquidfn.core
                           :optimizations :advanced
                           :pretty-print false}}]})
