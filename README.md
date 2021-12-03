# hex-arch

Provides macros for developing [hexagonal architectures](https://netflixtechblog.com/ready-for-changes-with-hexagonal-architecture-b315ec967749) in Rust services.

As of 0.3.0 are two main components provided:
- a `repositories` macro which uses a custom but relatively simple syntax to describe relations and mutability of user defined entities (your business logic items); the output of this macro is a set of traits which provide an ergonomic api for loading arbitrarily nested data based off their relationships
- an `adaptor` macro which generates implementations of the traits produced from the `repositories` macro; this macro uses a slightly more complex custom syntax to achieve its output, but with the positive tradeoff that there is a great deal of overrideability / flexibility provided on top of a relatively comprehensive default behavior
