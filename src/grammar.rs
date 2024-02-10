use crate::{cursor::Cursor, stream::DecodeError, Encoding};

mod scalar;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Context {
    BlockIn,
    BlockOut,
    BlockKey,
    FlowIn,
    FlowOut,
    FlowKey,
}

#[derive(Debug)]
pub(crate) enum State {
    Stream,
    DecodeError(DecodeError),
    Document {
        prev_terminated: bool,
    },
    DocumentEnd,
}

pub fn production<'s>(cursor: Cursor /* parser/state/n/c? */) {}
