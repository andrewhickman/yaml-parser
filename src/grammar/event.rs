use alloc::vec::Vec;

use crate::{
    char,
    cursor::Cursor,
    grammar::{
        document::{self, Document},
        flow, scalar, Context, State,
    },
    parser::Buffer,
    CollectionStyle, Diagnostic, Event, Receiver, ScalarStyle, Span,
};

use super::{
    tag::{self, Properties},
    Indent,
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
        if document.explicit || document::suffix(cursor, receiver)?.is_empty() {
            let version = document.version.clone();
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
        document,
        Indent::NONE,
        Context::BlockIn,
        document.explicit,
        false,
    )
}

fn block_value<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    buffer: &mut Buffer<'s>,
    states: &mut Vec<State<'s>>,
    document: &Document<'s>,
    indent: Indent,
    context: Context,
    allow_empty: bool,
    allow_compact: bool,
) -> Result<(Event<'s>, Span), Diagnostic> {
    let start = cursor.location();

    let Properties { anchor, tag } =
        tag::properties(cursor, receiver, document, indent.next(), context)?;

    if cursor.is_char(char::LITERAL)? {
        let value = scalar::literal(cursor, receiver, indent)?;
        return Ok((
            Event::Scalar {
                style: ScalarStyle::Literal,
                value,
                anchor,
                tag,
            },
            cursor.span(start),
        ));
    } else if cursor.is_char(char::SINGLE_QUOTE)? {
        let value = scalar::single_quoted(cursor, receiver, indent)?;
        return Ok((
            Event::Scalar {
                style: ScalarStyle::SingleQuoted,
                value,
                anchor,
                tag,
            },
            cursor.span(start),
        ));
    } else if cursor.is_char(char::DOUBLE_QUOTE)? {
        let value = scalar::double_quoted(cursor, receiver, indent)?;
        return Ok((
            Event::Scalar {
                style: ScalarStyle::DoubleQuoted,
                value,
                anchor,
                tag,
            },
            cursor.span(start),
        ));
    } else if cursor.is_char(char::SEQUENCE_START)? {
        let span = flow::sequence_start(cursor, receiver)?;

        return Ok((
            Event::SequenceStart {
                style: CollectionStyle::Flow,
                anchor,
                tag,
            },
            span,
        ));
    } else if cursor.is_char(char::MAPPING_START)? {
        let span = flow::mapping_start(cursor, receiver)?;

        return Ok((
            Event::MappingStart {
                style: CollectionStyle::Flow,
                anchor,
                tag,
            },
            span,
        ));
    }

    todo!()
}
