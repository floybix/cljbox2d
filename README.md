# cljbox2d

A [clojure](http://clojure.org/) wrapper for
[JBox2D](http://www.jbox2d.org/), which is a close Java port of Erin
Catto's excellent C++ [Box2D](http://www.box2d.org/) physics engine.

## Warning

Do not use this yet, the API is in flux.

## API docs

See http://floybix.github.com/cljbox2d/

## Usage

Some ports of the Box2D / JBox2D testbed tests:

<pre>
lein run -m cljbox2d.tests.varying-restitution
lein run -m cljbox2d.tests.revolute
lein run -m cljbox2d.tests.web
lein run -m cljbox2d.tests.collision-processing
lein run -m cljbox2d.tests.sensor-test
</pre>

See the ball bounce (requires some imagination...)

```clojure
(use 'cljbox2d.core)
(create-world!)
(def ground (body! {:type :static}
                   {:shape (edge [-40 0] [40 0])}))
(def ball (body! {:position [0 10]}
                 {:shape (circle 1), :restitution 0.5}))
;; let it go
(dotimes [i 10]
  (println (world-point ball))
  (step! (/ 1 3)))

;  [0.0 10.0]
;  [0.0 8.888889]
;  [0.0 6.8888893]
;  [0.0 4.8888893]
;  [0.0 2.8888893]
;  [0.0 1.0048437]   <- bounce!
;  [0.0 2.5603995]
;  [0.0 3.0048442]
;  [0.0 2.3381777]
;  [0.0 1.0048437]
```

## License

Copyright © 2012 Felix Andrews

Distributed under the Eclipse Public License, the same as Clojure.
