use core::f32::consts::PI;

use alloc::collections::VecDeque;

use crate::{cursor::Cursor, parser::Buffered, stream::DecodeError, Error, Event, Location, Span};

mod scalar;

#[derive(Debug, Clone)]
pub(crate) enum State {
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

pub(crate) fn event<'s>(
    states: &mut Vec<State>,
    buffer: &mut VecDeque<Buffered<'s>>,
    cursor: &mut Cursor<'s>,
) {
    let Some(state) = states.pop() else {
        return;
    };

    match state {
        State::Stream => stream_start(states, buffer, cursor),
        State::Document { prev_terminated } => todo!(),
        State::DocumentNode {
            allow_empty,
            allow_compact,
            indent,
            context,
        } => todo!(),
        State::DocumentEnd => todo!(),
        State::BlockSequence {
            indent,
            context,
            first,
        } => todo!(),
        State::FlowSequence {
            indent,
            context,
            first,
        } => todo!(),
        State::BlockMapping {
            indent,
            context,
            first,
        } => todo!(),
        State::FlowMapping {
            indent,
            context,
            first,
        } => todo!(),
        State::BlockMappingValue {
            explicit,
            indent,
            context,
        } => todo!(),
        State::FlowMappingValue {
            allow_adjacent,
            allow_empty,
            indent,
            context,
        } => todo!(),
        State::FlowPair { indent, context } => todo!(),
        State::FlowPairEnd { indent, context } => todo!(),
    }
}

fn stream_start<'s>(
    states: &mut Vec<State>,
    buffer: &mut VecDeque<Buffered<'s>>,
    cursor: &mut Cursor,
) {
    let encoding = match cursor.encoding() {
        Ok(encoding) => encoding,
        Err(error) => return buffer.push_back(Buffered::Error { error }),
    };
    let span = cursor.empty_span();

    states.push(State::Document {
        prev_terminated: true,
    });

    buffer.push_back(Buffered::Event {
        event: Event::StreamStart { encoding },
        span,
    });
}
