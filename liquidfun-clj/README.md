# liquidfun-clj

2D physics engine.

A [Clojure](https://clojure.org/) wrapper for
[LiquidFun](http://google.github.io/liquidfun/)
(via [JavaCPP](https://github.com/bytedeco/javacpp)).


## API Docs

TODO


## Examples

Some examples can be run from source using Leiningen as:

<pre>
cd liquidfun-clj.testbed/
lein run -m org.nfrac.liquidfun.testbed.tests.sensor-test/run
lein run -m org.nfrac.liquidfun.testbed.tests.particles/run
lein run -m org.nfrac.liquidfun.testbed.tests.elastic-particles/run
lein run -m org.nfrac.liquidfun.testbed.tests.rope-joint/run
lein run -m org.nfrac.liquidfun.testbed.tests.raycast/run
...etc...
</pre>


## License

Copyright © 2017 Felix Andrews

Distributed under the Eclipse Public License, the same as Clojure.
