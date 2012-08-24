
* want to have thread-safe worlds, testbed etc:
  * (so can run parallel simulations in separate worlds)
  * so use vars with thread-local bindings, not atoms
  * but is JBox2D thread-safe? (lots of pooling...)
* it should be easier to specify body shapes and join them together.
  * point-in-direction [angle] [angle fraction]
    * [keyword :n :ne :e :se etc]
    * are these in world reference or local reference?
    * see Shape/getSupport and Shape/getSupportVertex
  * point-furthest-from [other-point]
* calculate total force/work from motor torque:
  * Force(N) = torque(N.m) / distance(m)   (?)
* how to detect unmatched keys in argument maps?
* how to provide arbitrary contact handlers?
* testbed: set a default contact handler that:
  * buffers contacts (preSolve)
  * stores touching status for sensors (beginContact) (endContact)
* fixture functions:
  * isSensor
  * get/set friction, density, restitution etc
  * test point inside fixture
  * filter (getFilterData)
* more testbed tests
  * OneSided
  * TheoJansenWalker
  * Chain
  * Box2D: Bridge
  * Box2D: Pinball
  * Box2D: Car
  * Box2D: ConveyorBelt
  * Raycast
  * Pulleys
  * Cantilever
* testbed draw circle angle
* testbed restart 'r'
* GUI widgets to pause, step, restart etc
* need a library of shape functions?
  * e.g. transform polygons
  * counter-clockwise, convex construction
* scroll to zoom in/out
* testbed bomb (space)
* box2d thread?
