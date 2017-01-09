
* gen-class for callbacks?

* more joints
- friction
- gear
- motor
- pulley
- wheel

* make sure we copy/clone any Vec2 (etc) that is set on C++ objects
  because anything created on java side will be auto deallocated!

- FixtureDef::shape
- ParticleGroupDef::shape

- create FixtureDef (as subclass) holding a ref to Shape
  - then Shape won't be GC'd until after FixtureDef is GC'd

* "listener is owned by you and must remain in scope"

* body-seq joint-seq etc are unsafe wrapping mutable list?
