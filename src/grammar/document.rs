use crate::{char, cursor::Cursor, parser::Buffer, Error, Receiver, Token};

use super::token_char;

pub(super) fn prefix<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    buffer: &mut Buffer<'s>
) -> Result<(), Error> {
    token_char(cursor, receiver, Token::ByteOrderMark, char::BYTE_ORDER_MARK)?;


    Ok(())
}
