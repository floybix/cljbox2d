(ns org.nfrac.liquidfun.testbed.app
  (:require [org.nfrac.liquidfun.testbed.tests.chain :as chain]
            [org.nfrac.liquidfun.testbed.tests.collision-filtering :as cf]
            [org.nfrac.liquidfun.testbed.tests.collision-processing :as cp]
            [org.nfrac.liquidfun.testbed.tests.conveyor-belt :as cb]
            [org.nfrac.liquidfun.testbed.tests.edge-points :as ep]
            [org.nfrac.liquidfun.testbed.tests.elastic-particles :as el-part]
            [org.nfrac.liquidfun.testbed.tests.liquid-timer :as lt-part]
            [org.nfrac.liquidfun.testbed.tests.one-sided :as os]
            [org.nfrac.liquidfun.testbed.tests.particles :as part]
            [org.nfrac.liquidfun.testbed.tests.raycast :as ray]
            [org.nfrac.liquidfun.testbed.tests.revolute :as rev]
            [org.nfrac.liquidfun.testbed.tests.rope-joint :as rope]
            [org.nfrac.liquidfun.testbed.tests.sensor-test :as sens]
            [org.nfrac.liquidfun.testbed.tests.slider-crank :as sc]
            [org.nfrac.liquidfun.testbed.tests.surface-tension :as st-part]
            [org.nfrac.liquidfun.testbed.tests.varying-restitution :as rest]
            [org.nfrac.liquidfun.testbed.tests.web :as web]))

(defn ^:export test-select-handler
  [test]
  (case test
    "chain" (chain/run)
    "cf" (cf/run)
    "cp" (cp/run)
    "cb" (cb/run)
    "ep" (ep/run)
    "el-part" (el-part/run)
    "lt-part" (lt-part/run)
    "os" (os/run)
    "part" (part/run)
    "ray" (ray/run)
    "rev" (rev/run)
    "rope" (rope/run)
    "sens" (sens/run)
    "sc" (sc/run)
    "st-part" (st-part/run)
    "rest" (rest/run)
    "web" (web/run)))

(defn on-js-reload
  [])
