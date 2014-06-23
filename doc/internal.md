This is my big bulleted list scratchpad of how Swift might translate to the JVM.

1. General
  1. Each swift file is considered in the package of the folder it's in, relative to the source base
  1. There is no separate runtime library, pieces are compiled with the JAR every time
  1. Default compiler behavior is always what Apple's impl does, no matter how right/wrong
  1. Cannot, by default, use any API's not in the Android set
  1. Top-level
    1. Anything not in a data structure is put in a class at the package root called `package`
    1. Functions and vars and other declarations are put visible in the class
    1. Regular code is in the static initializer
  1. Compile-time plugin design
    1. Basically all of the stdlib is implemented in the compiler and inlined
    1. The pieces are probably named the same methods, not sure yet
    1. A context is provided so each piece can use it to create what it needs
  1. Runtime library
    1. None required
    1. Annotations added for all sorts of things. Should be class retention.
      1. `@Enum` - Either the outer interface for associated-value enum or the field for raw-value enum
      1. `@Function` - A closure class that's a Java 8-style functional interface
      1. `@Param` - For external names, marking inout, etc
      1. `@Struct` - A struct class
      1. `@Tuple` - A swift tuple class
      1. `@Type` - The swift type of something
      1. `@TypeAliases` - A collection of type aliases used by the package  
1. The Basics
  1. Constants and Variables
    1. `let` variables are final, `var` are not
    1. Most naming is the same as in Java. Escape what's not.
    1. Nothing over UTF16 supported currently
  1. Integers
    1. Overflow checks are inlined, see Guava *Math classes for examples
    1. Swift Int is java int. Compiler flag can force it to be long since swift impl is machine dependent
    1. All other types need `@Type` annotation on their variables
  1. Floating-Point Numbers
    1. Matches Java types. Default is double for type inference.
  1. Type Aliases
    1. Stored at package level with `@TypeAliases` annotation
  1. Tuples
    1. Stored as final class at highest non-instance level. If in a top class, store as static class there. Go up to package if necessary.
    1. Try to come up with best name based on context
    1. Custom class not needed when only used locally and not passed or returned anywhere
    1. Use property names instead of `propertyX` (where X is 0-based index) if named
    1. Have `get(int index)` with switch to get the property of a certain index at runtime for Java folks
    1. An empty tuple `()` is Java `Void` type in both directions
  1. Optionals
    1. Java interop (non swift types):
      1. All Java primitives and Void are non-optional
      1. AtomicReference and WeakReference are optional
      1. All other Java objects (including string and primitive wrappers) are implicitly unwrapped optionals
    1. If it doesn't match the Java type properly based on above, `@Type` is required on var
    1. Null checks for non-optionals are required whenever using as non-@Type parameter/var w/ existing Java and not under a null guard check in code
    1. Nil is Java null
  1. Assertions
    1. Do not use Java `assert` by default, use a regular conditional, but still throw `AssertionError`. Compiler flag can change this.
1. Basic Operators
  1. Assignment Operator
    1. Destructuring is no prob
  1. Arithmetic Operators
    1. We do overflow checks
  1. Comparison Operators
    1. `==` in swift world is `.equals` in Java world where appropriate
    1. `===` in swift world is `==` in Java world
  1. Range Operators
    1. Try to unroll in loop
    1. For other uses, may have to create anonymous AbstractList<Integer> live
1. Strings and Characters
  1. UTF16 is greatest char supported
  1. See how the identity comparison operator works here
1. Collection Types
  1. Arrays
    1. Java interop:
      1. Can work with regular arrays as immutable swift arrays
      1. There is no mutable array equivalent due to COW semantics, but List still has subscript support and casting to array will make swift version
    1. Mutable arrays are ArrayList
      1. COW semantics require deep-value copy sadly. And since we don't have a runtime library we have to find some way to make this clean, it may be too expensive/shareable/reusable to inline. I am stuck...
      1. Since there is no mutable array equiv for Java libs, you simply can't get one of these back from a Java lib. And all of these must have `@Type` annotation no matter what
    1. Immutable arrays are regular Java arrays
  1. Dictionaries
    1. Java interop:
      1. Since COW is needed can't convert from Java map unless done w/ `as`
    1. Dictionaries are HashMaps, immutable ones made as such
    1. All require `@Type` annotation
    1. Same deep copy problem as arrays
1. Control Flow
  1. For Loops
    1. Basically just like Java. Just make sure to unroll ranges
  1. Switch
    1. Try hardest to use Java one, otherwise make if/else (kinda, use labels because of possible fall through). Similar to what Scala does here.
    1. Pattern match impl needed here
1. Functions
  1. Defining and Calling Functions
    1. Java interop:
      1. We will also adopt the "functional interface" approach of Java 8 for closures
    1. Defining function at top level (i.e. not methods) are created statically at package level
    1. Using function signature (like closures) follows almost exactly like tuples:
      1. Create static class fewest way up the declaration stack and try to create best name
      1. Must be a single functional interface, so defaults are resolved with overloads in the class
      1. Classes marked with `@Function`
  1. Parameters
    1. Always final unless "var"
    1. External parameter names
      1. If given, external parameter names are also the local param names in JVM debug info...who cares about the actual local name
      1. If given, external parameter names are labelled with an annotation in case debug info is unavailable
      1. No existing Java libraries are considered to have external parameter names
    1. Default parameter values
      1. On closure functional interfaces, these are overloads
      1. Otherwise these are overloads where they are.
      1. Ambiguity detection required by compiler when creating these.
      1. For defaults that are not literals, they will be closures so they can be lazily invoked.
    1. In-out parameters
      1. Marked as inout=true in `@Param`
      1. It is passed as an `AtomicReference`
  1. Nested
    1. If not sent anywhere, will be inlined
1. Closures
  1. For the sort example, the sort-implementation will make the closure-to-comparator conversion and use the Arrays.sort or Collections.sort after copying
  1. TODO: figure out how to maintain capture. Probably an atomic reference. Must review JVM possibilities.
1. Enumerations
  1. Java interop:
    1. Works the same as non-value enum
  1. Non-value enums work just like Java enums. 
  1. Raw value enums:
    1. A final property on the Java enum annotated with `@Enum` to say it is the swift enum value
  1. Associated value enums:
    1. Outer interface annotated with `@Enum`
    1. Inner static classes for each case extending the outer interface
    1. Inner enum for the cases.
    1. The inner static classes need to be able to get the enum value and vice versa.
1. Classes and Structures
  1. Java interop:
    1. Everything is a class, no such thing as Java struct
    1. Properties try getter/setter first, then fall back to fields
  1. All properties are accessed via bean accessors
  1. Structs
    1. Final class implementing cloneable and annotated with `@Struct`
    1. Custom clone deeply clones...maybe. Sounds heavy, I'd like to wait until write.
    1. Memberwise initializer just instantiates and starts calling setters
    1. Setters are mutating methods basically. The caller is expected to clone properly.
1. Properties
  1. Let is final, var is not
  1. Let properties of structs need immutable = true on `@Type`
  1. Lazy is created on getter call
  1. Computed properties are all in the getter/setter. No fields.
  1. Property observers have code put in the setter (including inherited classes). Before or after the actual setting depending upon willSet or didSet
  1. Type properties are just static 
1. Methods
  1. Mostly as expected, and work like functions
  1. self is this
  1. Mutating methods
    1. Ones that just alter properties do as expected
    1. Ones that alter self and don't return a value return self
    1. Ones that alter self and do return a value accept an atomic reference as the first parameter
    1. Must have mutating = true in `@Function`
  1. Type methods are just static
1. Subscripts
  1. Java interop:
    1. `get` w/ at least one param can be accessed via subscript
    1. `set` or `put` w/ at least two params and no return value can be set via subscript with last being the value (special case made for maps/lists)
  1. Just methods called `get` and `set` unless they already exist. Gotta be careful here with inheritance.
  1. Must have subscript = true on `@Function`
1. Inheritance
  1. Mostly as expected with Java
  1. Initializers not inherited by default, heads up
  1. Property inheritance works just like overriding get/set
1. Initialization
  1. Basically constructors by any other name
  1. Overloads supported
  1. Unlike Java final properties, you can set the same final property many times in a swift constructor. Need a temp variable is appears.
1. Deinitialization
  1. a.k.a finalizer
1. Automatic Reference Counting
  1. We don't have the same problem on the JVM
  1. Not sure yet if I want to make `weak` as a `WeakReference` yet. I suspect I do
  1. We will ignore unowned for now, making it a noop
1. Optional Chaining
  1. Just null checks
1. Type Casting
  1. `is` will work like instanceof 
  1. Casting with optional support is basically catch a class cast exception
1. Nested Types
  1. Non-static inner classes basically
  1. Some special consideration needs to be take for enum because they are static by default in Java. Not sure if the JVM verifier will break if I make inner enum non-static
1. Extensions
  1. These have to be resolved at compile time for type evaluation. They also have to be resolved at runtime for protocol conformance.
  1. All extensions of a type must take up a single line in META-INF/gulliver-extensions/fully.qualified.ClassName (like Java ServiceLoaders). The line must be the fully qualified extension name. If any protocols are on the extension, a colon must follow the extension class name and then they must be listed comma-separated.
    1. This is needed at runtime for "is" and "as" on protocol checks. How am I gonna store it where it is globally accessible?
      1. Well, I could have a cache at each package level it is needed, but lots of duplication there. Maybe only for the protocols I need.
  1. Each extension is a class that cannot be constructed
  1. The compiler is required to resolve the extension as necessary.
  1. There is no java interop in any direction
  1. Computed Properties
    1. Static getters/setters take the instance of the extended item as first param
  1. Initializers
    1. Implemented as static `new` + extended class name
  1. Methods
    1. Just static method with instance as first parameter
    1. Mutating ones are still static and return the instance if didn't return before, or take an an atomic reference as second parameter
  1. Nested types
    1. Just static inner classes that accept the instance as the first param of the constructor (if the nested types are not static, still need to investigate)
1. Protocols
  1. Java interfaces and protocols are interchangeable and basically the same thing.
  1. An extension can even implement an interface for a swift type
  1. With Extension
    1. See above
  1. Protocol composition
    1. We will force this to be a generic constraint

TODO: more