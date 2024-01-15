use transpare_macros::Transpare;
mod named_fields {
    use super::*;
    #[derive(Transpare, PartialEq, Eq, Debug)]
    struct Something<ValueOne, ValueTwo> {
        pub something: ValueOne,
        pub something_else: ValueTwo,
    }

    #[test]
    fn test_map() {
        assert_eq!(
            Something { something: "asd", something_else: "" }
                .map_value_one(|value| value.len())
                .map_value_two(|_| ()),
            Something { something: 3, something_else: () }
        );
    }
    #[test]
    fn test_map_some() {
        assert_eq!(
            Something { something: "asd", something_else: "" }
                .map_some_value_one(|value| Some(value.len()))
                .and_then(|v| { v.map_some_value_two(|_| Some(())) }),
            Some(Something { something: 3, something_else: () })
        );
    }
    #[test]
    fn test_try_map() {
        assert_eq!(
            Something { something: "asd", something_else: "" }
                .try_map_value_one(|value| Result::<_, ()>::Ok(value.len()))
                .and_then(|something| { something.try_map_value_two(|_| Ok(())) }),
            Ok(Something { something: 3, something_else: () })
        );
    }
}
mod unnamed_fields {
    use super::*;

    #[derive(Transpare, PartialEq, Eq, Debug)]
    struct Something<ValueOne, ValueTwo>(ValueOne, ValueTwo);

    #[test]
    fn test_map() {
        assert_eq!(
            Something("asd", "").map_value_one(|value| value.len()).map_value_two(|_| ()),
            Something(3, ())
        );
    }
    #[test]
    fn test_map_some() {
        assert_eq!(
            Something("asd", ()).map_some_value_one(|value| Some(value.len())),
            Some(Something(3, ()))
        );
    }
    #[test]
    fn test_try_map() {
        assert_eq!(
            Something("asd", "")
                .try_map_value_one(|value| Result::<_, ()>::Ok(value.len()))
                .and_then(|something| { something.try_map_value_two(|_| Ok(())) }),
            Ok(Something(3, ()))
        );
    }
}
mod named_enum {
    use super::*;

    #[derive(Transpare, PartialEq, Eq, Debug)]
    enum Something<ValueOne> {
        One { something: ValueOne },
    }

    #[test]
    fn test_map() {
        assert_eq!(
            Something::One { something: "asd" }.map_value_one(|value| value.len()),
            Something::One { something: 3 }
        );
    }
    #[test]
    fn test_map_some() {
        assert_eq!(
            Something::One { something: "asd" }.map_some_value_one(|value| Some(value.len())),
            Some(Something::One { something: 3 })
        );
    }
    #[test]
    fn test_try_map() {
        assert_eq!(
            Something::One { something: "asd" }
                .try_map_value_one(|value| Result::<_, ()>::Ok(value.len())),
            Ok(Something::One { something: 3 })
        );
    }
}
mod unnamed_enum {
    use super::*;

    #[derive(Transpare, PartialEq, Eq, Debug)]
    enum Something<ValueOne> {
        One(ValueOne),
    }

    #[test]
    fn test_map() {
        assert_eq!(Something::One("asd").map_value_one(|value| value.len()), Something::One(3));
    }
    #[test]
    fn test_map_some() {
        assert_eq!(
            Something::One("asd").map_some_value_one(|value| Some(value.len())),
            Some(Something::One(3))
        );
    }
    #[test]
    fn test_try_map() {
        assert_eq!(
            Something::One("asd").try_map_value_one(|value| Result::<_, ()>::Ok(value.len())),
            Ok(Something::One(3))
        );
    }
}
