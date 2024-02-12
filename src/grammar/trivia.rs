use crate::{cursor::Cursor, parser::Buffer, Error, Event, Receiver, Span, Token};



pub fn comment_lines<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
) -> Result<(Event<'s>, Span), Error> {

}

pub fn try_separate_in_line<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>,
) -> Result<bool, Error> {
    match buffer.peek_token() {
        Some((Token::Indent, span)) if span.column => {
        },
        Some((Token::Separator, _)) => return Ok(true),
        _ => (),
    }

    todo!()
}