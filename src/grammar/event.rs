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
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State>,
) -> Option<Result<(Event<'s>, Span), Diagnostic>> {
    let Some(state) = states.pop() else {
        return None;
    };

    let res = match state {
        State::Error(err) => Err(err),
        State::Stream => stream(cursor, states),
        State::Document { prev_terminated } => {
            document(cursor, receiver, buffer, states, prev_terminated)
        }
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
    };

    if res.is_err() {
        // todo recovery
    }

    Some(res)
}

fn stream<'s>(
    cursor: &mut Cursor<'s>,
    states: &mut Vec<State>,
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
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State>,
    prev_terminated: bool,
) -> Result<(Event<'s>, Span), Diagnostic> {
    cursor.enter_document();

    loop {
        document::prefix(cursor, receiver, prev_terminated)?;
    }

    todo!()
}
