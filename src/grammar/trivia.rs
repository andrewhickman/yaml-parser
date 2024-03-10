#[cfg(test)]
mod tests;

use crate::{char, cursor::Cursor, diag::Expected, Diagnostic, Receiver, Span, Token};

use super::recover;

pub(super) fn try_non_content_break(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<bool, Diagnostic> {
    if try_break(cursor)? {
        receiver.token(Token::Break, cursor.token());
        Ok(true)
    } else {
        Ok(false)
    }
}

pub(super) fn non_content_break(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    if try_non_content_break(cursor, receiver)? {
        Ok(())
    } else {
        Err(Diagnostic::expected_token(Token::Break, cursor))
    }
}

fn try_break(cursor: &mut Cursor) -> Result<bool, Diagnostic> {
    if cursor.eat_char('\r')? {
        cursor.eat_char('\n')?;
        Ok(true)
    } else if cursor.eat_char('\n')? {
        Ok(true)
    } else {
        Ok(false)
    }
}

pub(super) fn separator_lines(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    if try_separate_in_line(cursor, receiver)? && cursor.is_char(char::COMMENT)? {
        comment_text(cursor, receiver)?;
    }

    if !cursor.is_end_of_input()? && !try_non_content_break(cursor, receiver)? && !cursor.is_separated() {
        return Err(Diagnostic::expected_token(Token::Break, cursor));
    }

    comment_lines(cursor, receiver)
}

pub(super) fn comment_lines(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    while try_separate_in_line(cursor, receiver)? {
        if cursor.is_char(char::COMMENT)? {
            comment_text(cursor, receiver)?;
        } else if !try_non_content_break(cursor, receiver)? || cursor.is_end_of_input()? {
            break;
        }
    }

    Ok(())
}

pub(super) fn comment_text(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<(), Diagnostic> {
    if !cursor.eat_char(char::COMMENT)? {
        return Err(Diagnostic::expected_token(Token::Comment, cursor));
    }
    cursor.eat_while(char::non_break)?;

    receiver.token(Token::Comment, cursor.token());

    if try_non_content_break(cursor, receiver)? || cursor.is_end_of_input()? {
        Ok(())
    } else {
        recover(
            cursor,
            receiver,
            Diagnostic::expected(Expected::Printable, cursor),
            |cursor| cursor.is(char::r#break),
        )?;
        try_non_content_break(cursor, receiver)?;
        Ok(())
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
        while cursor.is(char::space)? {
            cursor.bump();
        }
        receiver.token(Token::Separator, cursor.token());
        Ok(true)
    } else {
        Ok(cursor.is_separated())
    }
}
