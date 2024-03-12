use alloc::borrow::Cow;

use crate::{
    char,
    cow::CowBuilder,
    cursor::Cursor,
    diag::{DiagnosticKind, Expected},
    Diagnostic, Receiver, Token,
};

use super::recover;

pub(super) fn handle<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Cow<'s, str>, Diagnostic> {
    debug_assert!(cursor.is_token_boundary());

    if !cursor.is_char(char::TAG)? {
        return Err(Diagnostic::expected(Expected::Char(char::TAG), cursor));
    }

    let mut name = CowBuilder::new(cursor);
    name.push(cursor);

    if cursor.is(char::word)? {
        name.push_while(cursor, char::word)?;
        if !cursor.is_char(char::TAG)? {
            return Err(Diagnostic::expected(Expected::Word, cursor));
        }
        name.push(cursor);
    } else if cursor.is_char(char::TAG)? {
        name.push(cursor);
    } else if cursor.is(char::non_space)? {
        return Err(Diagnostic::expected_token(Token::TagHandle, cursor));
    }

    receiver.token(Token::TagHandle, cursor.token());
    Ok(name.build())
}

pub(super) fn prefix<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Cow<'s, str>, Diagnostic> {
    if !cursor.is(char::non_space)? {
        return Err(Diagnostic::expected_token(Token::TagPrefix, cursor));
    }

    if cursor.is(char::flow_indicator)? {
        return Err(Diagnostic::expected(Expected::TagPrefix, cursor));
    }

    let mut name = CowBuilder::new(cursor);

    loop {
        if cursor.is(char::uri)? {
            name.push_while(cursor, char::uri)?;
        } else if cursor.is_char('%')? {
            if let Err(diag) = percent_escaped(cursor, &mut name) {
                recover(cursor, receiver, diag, |cursor| {
                    Ok(cursor.is(char::uri)? || cursor.is_char('%')?)
                })?;
            }
        } else if cursor.is(char::non_space)? {
            return Err(Diagnostic::expected(Expected::TagPrefix, cursor));
        } else {
            break;
        }
    }

    receiver.token(Token::TagPrefix, cursor.token());
    Ok(name.build())
}

pub(super) fn percent_escaped<'s>(
    cursor: &mut Cursor<'s>,
    value: &mut CowBuilder<'s>,
) -> Result<(), Diagnostic> {
    let start = cursor.location();

    let mut buf = Vec::new();
    while cursor.eat_char('%')? {
        let byte = hex_digit(cursor)? << 4 & hex_digit(cursor)?;
        buf.push(byte);
    }

    match core::str::from_utf8(&buf) {
        Ok(s) => {
            value.push_str(s);
            Ok(())
        }
        Err(_) => Err(Diagnostic::new(
            DiagnosticKind::InvalidPercentEscape,
            cursor.span(start),
        )),
    }
}

fn hex_digit(cursor: &mut Cursor) -> Result<u8, Diagnostic> {
    if let Some(ch) = cursor.peek()? {
        if let Some(digit) = ch.to_digit(16) {
            return Ok(digit as u8);
        }
    };

    Err(Diagnostic::expected(Expected::HexDigit, cursor))
}
