use core::fmt;

use serde::Serialize;

use crate::{cursor::Cursor, stream::Stream, Diagnostic, Location, Receiver, Span, Token};

struct TestReceiver<'s, T> {
    stream: Option<&'s str>,
    index: usize,
    items: Vec<TestItem<T>>,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum TestItem<T> {
    Diagnostic {
        #[serde(flatten)]
        diag: Diagnostic,
        #[serde(skip_serializing_if = "Option::is_none")]
        stream: Option<String>,
    },
    Token {
        token: Token,
        #[serde(skip_serializing_if = "Option::is_none")]
        stream: Option<String>,
        span: Span,
    },
    Value {
        value: T,
    },
    Remainder {
        #[serde(skip_serializing_if = "Option::is_none")]
        remainder: Option<String>,
        span: Span,
    },
}

impl<'s, T> Receiver for TestReceiver<'s, T> {
    fn diagnostic(&mut self, diag: Diagnostic) {
        self.items.push(TestItem::Diagnostic {
            stream: self.stream(diag.span()),
            diag,
        })
    }

    fn token(&mut self, token: Token, span: Span) {
        self.items.push(TestItem::Token {
            token,
            stream: self.stream(span),
            span,
        })
    }
}

impl<'s, T> TestReceiver<'s, T> {
    fn new(cursor: &Cursor<'s>) -> Self {
        TestReceiver {
            items: Vec::new(),
            stream: cursor.as_str(),
            index: cursor.index(),
        }
    }

    fn stream(&self, span: Span) -> Option<String> {
        self.stream
            .map(|s| s[(span.start.index - self.index)..(span.end.index - self.index)].to_owned())
    }

    fn finish(&mut self, result: Result<T, Diagnostic>) {
        match result {
            Ok(value) => self.items.push(TestItem::Value { value }),
            Err(diag) => self.items.push(TestItem::Diagnostic {
                stream: self.stream(diag.span()),
                diag,
            }),
        }
    }
}

pub(super) fn parse<'s, T, F>(f: F, stream: &'s str) -> impl Serialize
where
    T: fmt::Debug + Serialize + 's,
    F: Fn(&mut Cursor<'s>, &mut (dyn Receiver + 's)) -> Result<T, Diagnostic>,
{
    parse_cursor(f, Cursor::new(Stream::from_str(stream)))
}

pub(super) fn parse_unseparated<'s, T, F>(f: F, stream: &'s str) -> impl Serialize
where
    T: fmt::Debug + Serialize + 's,
    F: Fn(&mut Cursor<'s>, &mut (dyn Receiver + 's)) -> Result<T, Diagnostic>,
{
    let mut cursor = Cursor::new(Stream::from_str("a"));
    assert_eq!(cursor.bump(), 'a');
    assert!(!cursor.is_separated());
    parse_cursor(f, cursor.with_stream(Stream::from_str(stream)))
}

pub(super) fn parse_indented<'s, T, F>(f: F, indent: u32, stream: &'s str) -> impl Serialize
where
    T: fmt::Debug + Serialize + 's,
    F: Fn(&mut Cursor<'s>, &mut (dyn Receiver + 's)) -> Result<T, Diagnostic>,
{
    let ident = " ".repeat(indent as usize);
    let mut cursor = Cursor::new(Stream::from_str(&ident));
    for i in 0..indent {
        assert_eq!(cursor.bump(), ' ');
    }
    assert!(cursor.is_separated());
    assert_eq!(cursor.indent(), Some(indent));
    parse_cursor(f, cursor.with_stream(Stream::from_str(stream)))
}

pub(super) fn parse_cursor<'s, T, F>(f: F, mut cursor: Cursor<'s>) -> impl Serialize
where
    T: fmt::Debug + Serialize + 's,
    F: Fn(&mut Cursor<'s>, &mut (dyn Receiver + 's)) -> Result<T, Diagnostic>,
{
    let init_cursor = cursor.clone();
    let mut receiver = TestReceiver::<T>::new(&cursor);

    let res = f(&mut cursor, &mut receiver);

    let mut location = Location::default();
    for item in &receiver.items {
        if let &TestItem::Token { span, .. } = item {
            assert_eq!(
                location, span.start,
                "tokens don't cover string {:?}: {:#?}",
                init_cursor, receiver.items
            );
            location = span.end;
        }
    }
    assert_eq!(
        location,
        cursor.location(),
        "tokens don't cover string {:?}: {:#?}",
        init_cursor,
        receiver.items
    );
    receiver.finish(res);

    if !cursor.is_end_of_input().unwrap() {
        let start = cursor.location();
        let remainder = cursor.as_str().map(ToOwned::to_owned);
        while !cursor.is_end_of_input().unwrap() {
            cursor.bump();
        }

        receiver.items.push(TestItem::Remainder {
            remainder,
            span: cursor.span(start),
        });
    }

    receiver.items
}
