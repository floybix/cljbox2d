
* body => body-of

* use euclidian.math.vector
* fully qualify namespace org.nfrac.cljbox2d
* edge-point-local
* in joint! give :limits []
* serialise worlds?
  * (fixture-spec) returns map from Fixture
  * (body-spec) returns map from Body
* how to detect unmatched keys in argument maps?
* fixture functions:
  * isSensor
  * get/set friction, density, restitution etc
  * test point inside fixture
  * filter (getFilterData)
* more testbed tests
  * ApplyForce
  * Car
  * Buoyancy
* testbed draw circle angle
* testbed restart 'r'
* need a library of shape functions?
  * e.g. transform polygons
  * counter-clockwise, convex construction
