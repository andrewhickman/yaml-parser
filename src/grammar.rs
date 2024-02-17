mod document;
mod event;
mod scalar;
mod trivia;

use crate::{cursor::Cursor, Diagnostic, Receiver, Token};

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

fn try_token_char<'s>(
    cursor: &mut Cursor<'s>,
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
