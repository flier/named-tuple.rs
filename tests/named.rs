use named_tuple::named_tuple;

named_tuple!(
    #[derive(Clone, Copy, Debug, Default, Hash, PartialEq)]
    Human<'a> {
        name: &'a str,
        age: usize,
    }
);

pub struct Foo {}
pub enum Bar {}

named_tuple!(
    pub Test<'a, 'b> {
        foo: &'a Foo,
        bar: &'b Bar,
    }
);

#[test]
fn test_name() {
    let human = Human::new("alice", 18);

    assert_eq!(human.name(), "alice");
    assert_eq!(human.age(), 18);

    assert_eq!((human.0).0, "alice");
    assert_eq!((human.0).1, 18);

    assert_eq!(format!("{:?}", human), "Human { name: \"alice\", age: 18 }");

    assert_eq!(human, ("alice", 18));

    assert_eq!(human, Human::from(("alice", 18)));

    let t: (&str, usize) = human.into();

    assert_eq!(t, ("alice", 18));
}
