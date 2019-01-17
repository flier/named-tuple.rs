use named_tuple::named_tuple;
use std::net::ToSocketAddrs;

named_tuple!(
    #[derive(Clone, Copy, Serialize, Deserialize)]
    struct Human<'a> {
        name: &'a str,
        age: usize,
    }
);

named_tuple!(
    #[derive(Clone, Copy, Serialize, Deserialize)]
    struct Endpoint(host, port);
);

#[test]
fn test_human() {
    let human: Human = ("alice", 18).into();

    assert_eq!(human.name(), "alice");
    assert_eq!(human.age(), 18);
    assert_eq!(human.field_values(), ("alice", 18));
}

#[test]
fn test_endpoint() {
    let endpoint: Endpoint<_, _> = ("localhost", 80).into(); // From<(...)>

    let addr = endpoint.to_socket_addrs().unwrap().collect::<Vec<_>>(); // Deref<Target=(...)>

    if endpoint != ("localhost", 443) {
        // PartialEq<(...)>
        if endpoint < ("localhost", 1024) {
            // PartialOrd<(...)>
            let (host, port) = endpoint.into(); // Into<(...)>
        }
    }
}
