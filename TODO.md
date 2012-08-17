
== Short-term ==

* functions for joints' limits and motors etc
  * protocols? Motorized and Limitable, Locatable
* don't need {body|fixture|joint}def methods, just use a map!
  * bypass *Def java objects altogether and just set up Object?
* how to provide arbitrary contact handlers?
* testbed: set a default contact handler that:
  * buffers contacts (preSolve)
  * stores touching status for sensors (beginContact) (endContact)
* more testbed tests
  * SliderCrank
  * Raycast
  * OneSided
  * TheoJansenWalker
  * Pulleys
  * Cantilever
  * Chain
* testbed draw circle angle
* testbed pause
* testbed restart 'r'
* GUI widgets to pause, step, restart etc
* need a library of shape functions?
  * e.g. transform polygons
  * counter-clockwise, convex construction
* scroll to zoom in/out
* testbed bomb (space)
* box2d thread?

== Long-term ==

* creature (defrecord?)
* creature movement strategies
* creature senses (positions, motor states, contacts, sensors/raycast)
* allow creatures to decide whether to pass through things or not
  * but some things are not passable
  * both creatures must agree to pass each other
* evolution (grammatical?)

