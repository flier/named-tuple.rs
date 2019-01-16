named-tuple.rs
==============

A macro for declaring a `struct` that manages a set of fields in a `tuple`.

[![Travis-CI Status](https://travis-ci.org/flier/named-tuple.rs.svg?branch=master)](https://travis-ci.org/flier/named-tuple.rs)
[![Latest version](https://img.shields.io/crates/v/named_tuple.svg)](https://crates.io/crates/named_tuple)
[![Documentation](https://docs.rs/named_tuple/badge.svg)](https://docs.rs/named_tuple)
[![License](https://img.shields.io/crates/l/named_tuple.svg)](https://github.com/flier/named-tuple.rs#license)

# Getting Started

[named-tuple.rs is available on crates.io](https://crates.io/crates/named_tuple).
It is recommended to look there for the newest released version, as well as links to the newest builds of the docs.

At the point of the last update of this README, the latest published version could be used like this:

Add the following dependency to your Cargo manifest...

```toml
[dependencies]
named_tuple = "0.1"
```

...and see the [docs](https://docs.rs/named_tuple) for how to use it.

# Example

```rust
#[macro_use]
extern crate named_tuple;

named_tuple!(
    #[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
    struct Human<'a> {
        name: &'a str,
        age: usize,
    }
);

named_tuple!(
    #[derive(Copy, Debug)]
    struct Endpoint(host, port);
);

fn main() {
    /// structs with named fields
    let mut human = Human::new("alice", 18);

    assert_eq!(Human::field_names(), &["name", "age"]);
    assert_eq!(human.fields(), (("name", "alice"), ("age", 18)));
    assert_eq!(human.field_values(), ("alice", 18));

    assert_eq!(human.name(), "alice");
    assert_eq!(human.age(), 18);

    assert_eq!((human.0).0, "alice");
    assert_eq!((human.0).1, 18);

    assert_eq!(format!("{:?}", human), "Human { name: \"alice\", age: 18 }");

    human.set_name("bob");
    assert_eq!(human, ("bob", 18));

    human.set_age(20);
    assert_eq!(human, Human::from(("bob", 20)));

    let t: (&str, usize) = human.into();

    assert_eq!(t, ("bob", 20));

    // tuple structs
    let mut endpoint = Endpoint::new("localhost", 80);

    assert_eq!(endpoint.field_names(), &["host", "port"]);
    assert_eq!(endpoint.fields(), (("host", "localhost"), ("port", 80)));
    assert_eq!(endpoint.field_values(), ("localhost", 80));

    assert_eq!(endpoint.host(), "localhost");
    assert_eq!(endpoint.port(), 80);

    assert_eq!(
        format!("{:?}", endpoint),
        "Endpoint { host: \"localhost\", port: 80 }"
    );

    endpoint.set_host("google.com");
    endpoint.set_port(443);

    assert_eq!(("google.com", 443), endpoint.into());
}
```

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
