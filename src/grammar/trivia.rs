#[cfg(test)]
mod tests;

use crate::{char, cursor::Cursor, diag::Expected, Diagnostic, Receiver, Span, Token};

pub(super) fn try_line_break(cursor: &mut Cursor) -> Result<bool, Diagnostic> {
    if cursor.eat_char('\r')? {
        cursor.eat_char('\n')?;
        Ok(true)
    } else if cursor.eat_char('\n')? {
        Ok(true)
    } else {
        Ok(false)
    }
}

pub(super) fn comment_lines(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    loop {
        if !try_separate_in_line(cursor, receiver)? {
            return Ok(());
        }
        if cursor.is_char(char::COMMENT)? {
            comment_text(cursor, receiver)?;
        } else if try_line_break(cursor)? || cursor.is_end_of_input()? {
            return Ok(());
        }
    }
}

pub(super) fn comment_text(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    let start = cursor.location();
    if !cursor.eat_char(char::COMMENT)? {
        return Err(Diagnostic::expected_token(Token::Comment, cursor));
    }
    cursor.eat_while(char::non_break)?;

    let end = cursor.location();
    receiver.token(Token::Comment, Span::new(start, end));

    if try_line_break(cursor)? {
        receiver.token(Token::Break, cursor.span(end));
        Ok(())
    } else if cursor.is_end_of_input()? {
        Ok(())
    } else {
        Err(Diagnostic::expected(Expected::Printable, cursor))
    }
}

pub(super) fn separate_in_line(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    if try_separate_in_line(cursor, receiver)? {
        Ok(())
    } else {
        Err(Diagnostic::expected_token(Token::Separator, cursor))
    }
}

pub(super) fn try_separate_in_line(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<bool, Diagnostic> {
    if cursor.is(char::space)? {
        let start = cursor.location();
        while cursor.is(char::space)? {
            cursor.bump();
        }
        receiver.token(Token::Separator, cursor.span(start));
        Ok(true)
    } else {
        Ok(cursor.is_separated())
    }
}
