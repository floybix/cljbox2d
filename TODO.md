

* it should be easier to specify body shapes and join them together.
  * point-in-direction [angle] [angle fraction] [keyword :n :ne :e :se etc]
    * see Shape/getSupport and Shape/getSupportVertex
  * point-furthest-from [other-point]
* functions for joints' limits and motors etc
  * protocols? Motorized and Limitable, Locatable
* calculate total force/work from motor torque:
  * Force(N) = torque(N.m) / distance(m)   (?)
* how to provide arbitrary contact handlers?
* testbed: set a default contact handler that:
  * buffers contacts (preSolve)
  * stores touching status for sensors (beginContact) (endContact)
* body functions:
  * getContactList?
  * apply linear/angular impulse?
  * get/set linear/angular damping? set velocity?
  * isSleepingAllowed? get/set awake? get/set active?
  * get/set bullet? get/set isFixedRotation?
* fixture functions:
  * isSensor
  * get/set friction, density, restitution etc
  * test point inside fixture
  * filter (getFilterData)
* more testbed tests
  * SliderCrank
  * OneSided
  * TheoJansenWalker
  * Raycast
  * Pulleys
  * Cantilever
  * Chain
* testbed draw circle angle
* testbed restart 'r'
* GUI widgets to pause, step, restart etc
* need a library of shape functions?
  * e.g. transform polygons
  * counter-clockwise, convex construction
* scroll to zoom in/out
* testbed bomb (space)
* box2d thread?
