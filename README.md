## 2D physics in Clojure

* **liquidfun-clj** - wraps the LiquidFun C++ library.
  * **liquidfun-clj.testbed** - testbed tests, uses Quil for drawing.
* **liquidfun-cljs** - wraps the LiquidFun Javascript library.
  * **liquidfun-cljs.testbed** - testbed tests, uses Quil for browser rendering.
* **cljbox2d** _(old)_ - wraps the JBox2D java port.
  * **cljbox2d.testbed** _(old)_ - testbed tests, uses Quil for drawing.

These expose a mostly identical API, but there are some differences due to the
different underlying platforms.
