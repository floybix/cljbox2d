(ns org.nfrac.liquidfn.testbed.app
  (:require [org.nfrac.liquidfn.testbed.tests.chain :as chain]
            [org.nfrac.liquidfn.testbed.tests.collision-filtering :as cf]
            [org.nfrac.liquidfn.testbed.tests.collision-processing :as cp]
            [org.nfrac.liquidfn.testbed.tests.conveyor-belt :as cb]
            [org.nfrac.liquidfn.testbed.tests.edge-points :as ep]
            [org.nfrac.liquidfn.testbed.tests.one-sided :as os]
            [org.nfrac.liquidfn.testbed.tests.raycast :as ray]
            [org.nfrac.liquidfn.testbed.tests.revolute :as rev]
            [org.nfrac.liquidfn.testbed.tests.rope-joint :as rope]
            [org.nfrac.liquidfn.testbed.tests.sensor-test :as sens]
            [org.nfrac.liquidfn.testbed.tests.slider-crank :as sc]
            [org.nfrac.liquidfn.testbed.tests.varying-restitution :as rest]
            [org.nfrac.liquidfn.testbed.tests.web :as web]))

(defn ^:export test-select-handler
  [test]
  (case test
    "chain" (chain/run)
    "cf" (cf/run)
    "cp" (cp/run)
    "cb" (cb/run)
    "ep" (ep/run)
    "os" (os/run)
    "ray" (ray/run)
    "rev" (rev/run)
    "rope" (rope/run)
    "sens" (sens/run)
    "sc" (sc/run)
    "rest" (rest/run)
    "web" (web/run)))

(defn on-js-reload
  [])
