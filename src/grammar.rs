mod document;
mod event;
mod scalar;
mod trivia;

use crate::{cursor::Cursor, Diagnostic, Location, Receiver, Span, Token};

pub(crate) use self::event::event;

#[derive(Debug, Clone)]
pub(crate) enum State {
    Error(Diagnostic),
    Stream,
    Document {
        prev_terminated: bool,
    },
    DocumentNode {
        allow_empty: bool,
        allow_compact: bool,
        indent: i32,
        context: Context,
    },
    DocumentEnd,
    BlockSequence {
        indent: i32,
        context: Context,
        first: bool,
    },
    FlowSequence {
        indent: i32,
        context: Context,
        first: bool,
    },
    BlockMapping {
        indent: i32,
        context: Context,
        first: bool,
    },
    FlowMapping {
        indent: i32,
        context: Context,
        first: bool,
    },
    BlockMappingValue {
        explicit: bool,
        indent: i32,
        context: Context,
    },
    FlowMappingValue {
        allow_adjacent: bool,
        allow_empty: bool,
        indent: i32,
        context: Context,
    },
    FlowPair {
        indent: i32,
        context: Context,
    },
    FlowPairEnd {
        indent: i32,
        context: Context,
    },
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Context {
    BlockIn,
    BlockOut,
    BlockKey,
    FlowIn,
    FlowOut,
    FlowKey,
}

fn try_token_char(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
    token: Token,
    ch: char,
) -> Result<bool, Diagnostic> {
    if cursor.is_char(ch)? {
        let start = cursor.location();
        cursor.bump();
        receiver.token(token, cursor.span(start));
        Ok(true)
    } else {
        Ok(false)
    }
}

fn token_char(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
    token: Token,
    ch: char,
) -> Result<(), Diagnostic> {
    let start = cursor.location();
    if cursor.eat_char(ch)? {
        receiver.token(token, cursor.span(start));
        Ok(())
    } else {
        Err(Diagnostic::expected_token(token, cursor))
    }
}

fn token(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
    token: Token,
    pred: impl Fn(char) -> bool + Clone,
) -> Result<Span, Diagnostic> {
    let start = cursor.location();
    while cursor.eat(pred.clone())? {}
    let span = cursor.span(start);
    receiver.token(token, span);
    Ok(span)
}

fn recover(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
    diag: Diagnostic,
    pred: impl Fn(&Cursor) -> Result<bool, Diagnostic>,
) -> Result<(), Diagnostic> {
    if diag.is_recoverable() {
        receiver.diagnostic(diag.clone());

        while !pred(cursor)? && !cursor.is_end_of_stream()? {
            cursor.bump();
        }
        let span = cursor.span(diag.span().start);
        if !span.is_empty() {
            receiver.token(Token::Error, span);
        }

        Ok(())
    } else {
        Err(diag)
    }
}
