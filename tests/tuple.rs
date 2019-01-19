#[cfg(feature = "serde")]
#[macro_use]
extern crate serde_derive;

use std::net::ToSocketAddrs;

use named_tuple::named_tuple;

named_tuple!(
    #[derive(Clone, Copy)]
    struct Human<'a> {
        name: &'a str,
        age: usize,
    }
);

named_tuple!(
    struct HumanByRef<'a> {
        name: &'a str,
        age: usize,
    }
);

#[cfg(feature = "serde")]
named_tuple!(
    #[derive(Clone, Copy, Debug, Default, Serialize, Deserialize)]
    struct Pair(first, second);
);

#[cfg(not(feature = "serde"))]
named_tuple!(
    #[derive(Clone, Copy, Debug, Default)]
    struct Pair(first, second);
);

named_tuple!(
    #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
    struct Endpoint<'a> {
        host: &'a str = "localhost", 
        port: u16 = 80,
    }
);

pub struct Foo {}
pub enum Bar {}

named_tuple!(
    pub struct Lifetimes<'a, 'b> {
        foo: &'a Foo,
        bar: &'b Bar,
    }
);

#[test]
fn test_human() {
    let mut human: Human = ("alice", 18).into();

    assert_eq!(human.name(), "alice");
    assert_eq!(human.age(), 18);

    assert_eq!(human.field_names(), &["name", "age"]);
    assert_eq!(human.fields(), (("name", "alice"), ("age", 18)));
    assert_eq!(human.field_values(), ("alice", 18));

    human.set_name("bob");
    human.set_age(20);
    assert_eq!(("bob", 20), human.into());
}

#[test]
fn test_human_by_ref() {
    let human = HumanByRef::new("alice", 18);

    assert_eq!(*human.name(), "alice");
    assert_eq!(*human.age(), 18);
    assert_eq!(human.field_values(), &("alice", 18));
}

#[test]
fn test_pair() {
    let mut pair: Pair<_, _> = ("foo", "bar").into();

    assert_eq!(pair.first(), "foo");
    assert_eq!(pair.second(), "bar");

    assert_eq!(pair.field_names(), &["first", "second"]);
    assert_eq!(pair.fields(), (("first", "foo"), ("second", "bar")));
    assert_eq!(pair.field_values(), ("foo", "bar"));

    pair.set_first("hello");
    pair.set_second("world");
    assert_eq!(("hello", "world"), pair.into());
}

#[cfg(feature = "serde")]
#[test]
fn test_serde() {
    let pair = Pair::new("foo", "bar");

    let json = serde_json::to_string(&pair).unwrap();

    assert_eq!(json, "[\"foo\",\"bar\"]");

    let pair2: Pair<_, _> = serde_json::from_str(&json).unwrap();

    assert_eq!(pair, pair2);
}

#[test]
fn test_endpoint() {
    let endpoint = Endpoint::default();

    let addr = endpoint.to_socket_addrs().unwrap().collect::<Vec<_>>(); // Deref<Target=(...)>

    assert_eq!(
        addr,
        vec!["[::1]:80".parse().unwrap(), "127.0.0.1:80".parse().unwrap()]
    );

    if endpoint != ("localhost", 443) {
        // PartialEq<(...)>
        if endpoint < ("localhost", 1024) {
            // PartialOrd<(...)>
            let (host, port) = endpoint.into(); // Into<(...)>

            assert_eq!(host, "localhost");
            assert_eq!(port, 80);
        }
    }
}
