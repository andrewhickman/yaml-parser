use crate::{char, cursor::Cursor, diag::Expected, Diagnostic, Receiver, Span, Token};

pub(super) fn try_line_break(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
) -> Result<Option<Span>, Diagnostic> {
    let start = cursor.location();
    if cursor.eat_char('\r')? {
        cursor.eat_char('\n')?;
        Ok(Some(cursor.span(start)))
    } else if cursor.eat_char('\n')? {
        Ok(Some(cursor.span(start)))
    } else {
        Ok(None)
    }
}

pub(super) fn comment_lines(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
) -> Result<(), Diagnostic> {
    loop {
        if !try_separate_in_line(cursor, receiver)? {
            return Ok(());
        }
        if cursor.is_char(char::COMMENT)? {
            comment_text(cursor, receiver)?;
        } else if try_line_break(cursor, receiver)?.is_none() {
            return Ok(());
        }
    }
}

pub(super) fn comment_text(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
) -> Result<(), Diagnostic> {
    let start = cursor.location();
    if !cursor.eat_char(char::COMMENT)? {
        return Err(Diagnostic::expected_token(Token::Comment, cursor));
    }
    loop {
        if cursor.is(char::r#break)? {
            let end = cursor.location();
            receiver.token(Token::Comment, Span::new(start, end));
            try_line_break(cursor, receiver)?;
            receiver.token(Token::Break, cursor.span(end));
            return Ok(());
        } else if cursor.is(char::non_break)? {
            cursor.bump();
        } else {
            return Err(Diagnostic::expected(Expected::Printable, cursor));
        }
    }
}

pub(super) fn separate_in_line(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
) -> Result<(), Diagnostic> {
    if try_separate_in_line(cursor, receiver)? {
        Ok(())
    } else {
        Err(Diagnostic::expected_token(Token::Separator, cursor))
    }
}

pub(super) fn try_separate_in_line(
    cursor: &mut Cursor,
    receiver: &mut impl Receiver,
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
