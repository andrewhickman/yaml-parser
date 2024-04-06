use alloc::vec::Vec;

use crate::{
    char,
    cursor::Cursor,
    diag::DiagnosticKind,
    grammar::{document, State},
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
        State::Document { prev_terminated } => {
            document(cursor, receiver, buffer, states, prev_terminated)
        }
        State::DocumentValue { document } => todo!(),
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
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State<'s>>,
    mut prev_terminated: bool,
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
