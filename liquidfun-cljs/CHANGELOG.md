# Change Log

## [Unreleased]
- Initial version of liquidfn in clojurescript, adapted from cljbox2d.

### Differences from cljbox2d:
- The js bindings refer to global state: a js var called "world".
- There is no constant-volume-joint, that was specific to JBox2D.
- destroy! replaced with destroy-body! destroy-joint! destroy-fixture!
- The following functionality is missing (from liquidfun.js)...
- **Bodies**
  - getLinearVelocityFromLocalPoint / WorldPoint missing.
  - mass, center etc all missing for Fixtures.
	- get/set GravityScale missing (but can set it in initial body def).
	- awake?
- **Joints**
  - joint-type
  - anchor-a, anchor-b.
	- body-a, body-b for all but revolute & primatic joints.
  - reaction-force, reaction-torque missing.
	- motor-torque, max-motor-torque, max-motor-force, power-watts
	- joint-speed missing, use joint-angular-velocity /
	- limits, limits! missing
  - motor-speed missing for revolute joint.
  - motor-speed! only works when the motor is off.
- **Contacts**
	- .isTouching check missing for contacts
  - .SetEnabled on contacts is missing.
  - current-contacts, contacting, all-current-contacts missing.
	  - solution: use a ContactListener.
  - manifold .-normal (have workaround)

## [0.5.0] - 2015-03-09
- cljbox2d release

[Unreleased]: https://github.com/floybix/cljbox2d/compare/v0.5.0...HEAD
[0.5.0]: https://github.com/floybix/cljbox2d/compare/v0.5.0...v0.5.0
