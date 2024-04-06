use alloc::vec::Vec;

use crate::{
    cursor::Cursor,
    grammar::{
        document::{self, Document},
        Context, State,
    },
    parser::Buffer,
    Diagnostic, Event, Receiver, Span,
};

pub(crate) fn event<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State<'s>>,
) -> Option<Result<(Event<'s>, Span), Diagnostic>> {
    let res = match states.pop()? {
        State::Error(err) => Err(err),
        State::Stream => stream(cursor, states),
        State::Document { prev_terminated } => document(cursor, receiver, states, prev_terminated),
        State::DocumentValue { document } => {
            document_value(cursor, receiver, buffer, states, &document)
        }
        State::DocumentEnd => todo!(),
        State::BlockSequence { .. } => todo!(),
        State::FlowSequence { .. } => todo!(),
        State::BlockMapping { .. } => todo!(),
        State::FlowMapping { .. } => todo!(),
        State::BlockMappingValue { .. } => todo!(),
        State::FlowMappingValue { .. } => todo!(),
        State::FlowPair { .. } => todo!(),
        State::FlowPairEnd { .. } => todo!(),
    };

    if res.is_err() {
        // todo recovery
    }

    Some(res)
}

fn stream<'s>(
    cursor: &mut Cursor<'s>,
    states: &mut Vec<State<'s>>,
) -> Result<(Event<'s>, Span), Diagnostic> {
    states.push(State::Document {
        prev_terminated: true,
    });

    Ok((
        Event::StreamStart {
            encoding: cursor.encoding(),
        },
        cursor.empty_span(),
    ))
}

fn document<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    states: &mut Vec<State<'s>>,
    prev_terminated: bool,
) -> Result<(Event<'s>, Span), Diagnostic> {
    while !cursor.is_end_of_input()? {
        let (document, span) = document::prefix(cursor, receiver, prev_terminated)?;
        if document.explicit() || document::suffix(cursor, receiver)?.is_empty() {
            let version = document.version().cloned();
            states.push(State::DocumentValue { document });
            return Ok((Event::DocumentStart { version }, span));
        }
    }

    Ok((Event::StreamEnd, cursor.empty_span()))
}

fn document_value<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State<'s>>,
    document: &Document<'s>,
) -> Result<(Event<'s>, Span), Diagnostic> {
    block_value(
        cursor,
        receiver,
        buffer,
        states,
        -1,
        Context::BlockIn,
        document.explicit(),
        false,
    )
}

fn block_value<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State<'s>>,
    indent: i32,
    context: Context,
    allow_empty: bool,
    allow_compact: bool,
) -> Result<(Event<'s>, Span), Diagnostic> {
    todo!()
}
