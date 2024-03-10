mod document;
mod event;
mod scalar;
#[cfg(test)]
mod tests;
mod trivia;

use crate::{cursor::Cursor, Diagnostic, Receiver, Span, Token};

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
    receiver: &mut (impl Receiver + ?Sized),
    token: Token,
    ch: char,
) -> Result<bool, Diagnostic> {
    debug_assert!(cursor.is_token_boundary());
    if cursor.eat_char(ch)? {
        receiver.token(token, cursor.token());
        Ok(true)
    } else {
        Ok(false)
    }
}

fn token_char(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
    token: Token,
    ch: char,
) -> Result<(), Diagnostic> {
    debug_assert!(cursor.is_token_boundary());
    if try_token_char(cursor, receiver, token, ch)? {
        Ok(())
    } else {
        Err(Diagnostic::expected_token(token, cursor))
    }
}

fn token(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
    token: Token,
    pred: impl Fn(char) -> bool + Clone,
) -> Result<Span, Diagnostic> {
    debug_assert!(cursor.is_token_boundary());
    while cursor.eat(pred.clone())? {}
    let span = cursor.token();
    receiver.token(token, span);
    Ok(span)
}

fn recover(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
    diag: Diagnostic,
    pred: impl Fn(&Cursor) -> Result<bool, Diagnostic>,
) -> Result<(), Diagnostic> {
    if diag.is_recoverable() {
        while !pred(cursor)? && !cursor.is_end_of_input()? {
            cursor.bump();
        }
        let span = cursor.token();
        if !span.is_empty() {
            receiver.token(Token::Error, span);
        }

        receiver.diagnostic(diag.clone());
        Ok(())
    } else {
        Err(diag)
    }
}