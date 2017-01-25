# Change Log

## [Unreleased]
- Initial version of liquidfun in clojure.

### Differences from cljbox2d:
- Particles, obviously.
- Runs compiled C++ code using JavaCPP.
  - Be careful with memory, don't use pointers that have been freed, etc.
- buffering-contact-listener returns [listener atom]; need to hold reference.
- There is no constant-volume-joint, that was specific to JBox2D.
- destroy! replaced with destroy-body! destroy-joint! destroy-fixture!
- user-data can only hold Pointer.
- fixture-mass, fixture-loc-center, fixture-radius.

## [0.5.0] - 2015-03-09
- cljbox2d release

[Unreleased]: https://github.com/floybix/cljbox2d/compare/v0.5.0...HEAD
[0.5.0]: https://github.com/floybix/cljbox2d/compare/v0.5.0...v0.5.0
