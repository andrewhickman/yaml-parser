use serde::Serialize;

use crate::{cursor::Cursor, stream::Stream, Diagnostic, Location, Receiver, Span, Token};

#[derive(Serialize)]
#[serde(transparent)]
struct TestReceiver<T> {
    items: Vec<TestItem<T>>,
}

#[derive(Serialize)]
#[serde(untagged)]
enum TestItem<T> {
    Diagnostic { diag: Diagnostic },
    Token { token: Token, span: Span },
    Value { value: T },
}

impl<T> Receiver for TestReceiver<T> {
    fn diagnostic(&mut self, diag: Diagnostic) {
        self.items.push(TestItem::Diagnostic { diag })
    }

    fn token(&mut self, token: Token, span: Span) {
        self.items.push(TestItem::Token { token, span })
    }
}

impl<T> TestReceiver<T> {
    fn new() -> Self {
        TestReceiver { items: Vec::new() }
    }

    fn finish(&mut self, result: Result<T, Diagnostic>) {
        match result {
            Ok(value) => self.items.push(TestItem::Value { value }),
            Err(diag) => self.items.push(TestItem::Diagnostic { diag }),
        }
    }
}

pub(super) fn parse<'s, T, F>(f: F, s: &'s str) -> impl Serialize + 's
where
    T: Serialize + 's,
    F: Fn(&mut Cursor<'s>, &mut (dyn Receiver + 's)) -> Result<T, Diagnostic>,
{
    let mut receiver = TestReceiver::<T>::new();
    let mut cursor = Cursor::new(Stream::from_str(s));

    let res = f(&mut cursor, &mut receiver);
    receiver.finish(res);

    let mut location = Location::default();
    let mut prev_token = None;
    for item in &receiver.items {
        if let &TestItem::Token { span, token } = item {
            assert_eq!(location, span.start);
            assert!(prev_token != Some(token) || token == Token::Error);
            location = span.end;
            prev_token = Some(token);
        }
    }
    assert_eq!(location, cursor.location());
    assert!(cursor.is_end_of_input().unwrap());

    receiver
}
