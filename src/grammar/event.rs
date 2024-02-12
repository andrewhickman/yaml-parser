use crate::{
    cursor::Cursor,
    grammar::{document, State},
    parser::Buffer,
    Error, Event, Receiver, Span,
};

pub(crate) fn event<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State>,
) -> Option<Result<(Event<'s>, Span), Error>> {
    let Some(state) = states.pop() else {
        return None;
    };

    let res = match state {
        State::Stream => stream(cursor, states),
        State::Document { prev_terminated } => document(cursor, receiver, buffer, states, prev_terminated),
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
) -> Result<(Event<'s>, Span), Error> {
    let encoding = match cursor.encoding() {
        Ok(encoding) => encoding,
        Err(error) => return Err(error),
    };
    let span = cursor.empty_span();

    states.push(State::Document {
        prev_terminated: true,
    });

    Ok((Event::StreamStart { encoding }, span))
}

fn document<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State>,
    prev_terminated: bool,
) -> Result<(Event<'s>, Span), Error> {
    cursor.enter_document();

    loop {
        document::prefix(cursor, receiver, buffer)?;
    }

    todo!()
}
