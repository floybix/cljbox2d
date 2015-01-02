
* use euclidian.math.vector
* bring in from uglyboids:
  * buffered-contact-listener
  * set-velocity
* fully qualify namespace org.nfrac.cljbox2d
* edge-point-local
* in joint! give :limits []
* serialise worlds?
  * (fixture-spec) returns map from Fixture
  * (body-spec) returns map from Body
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
  * Raycast
  * PolyShapes
  * BodyTypes
  * CharacterCollision
  * OneSided
  * ConveyorBelt
  * ApplyForce
* testbed draw circle angle
* testbed restart 'r'
* need a library of shape functions?
  * e.g. transform polygons
  * counter-clockwise, convex construction
