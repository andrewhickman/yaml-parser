use crate::{
    char, cursor::Cursor, diag::DiagnosticKind, parser::Buffer, Diagnostic, Receiver, Token,
};

use super::{trivia::comment_lines, try_token_char};

pub(super) fn prefix<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut impl Receiver,
    prev_terminated: bool,
) -> Result<(), Diagnostic> {
    try_token_char(
        cursor,
        receiver,
        Token::ByteOrderMark,
        char::BYTE_ORDER_MARK,
    )?;

    comment_lines(cursor, receiver)?;

    while cursor.is_char(char::DIRECTIVE)? {
        if !cursor.is_start_of_line() {
            // todo recover?
            // return Err(Error::new(ErrorKind::DirectiveNotAtStartOfLine, cursor.next_char_span()))
        }
        if !prev_terminated {
            // todo recover?
            // return Err(Error::new(ErrorKind::DirectiveAfterUnterminatedDocument, cursor.next_char_span()))
        }
    }
    // if cursor.is_char(ch)

    todo!()
}

// fn recover_directive
