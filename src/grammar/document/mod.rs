#[cfg(test)]
mod tests;

use alloc::{
    borrow::{Cow, ToOwned},
    collections::{btree_map, BTreeMap},
};

use crate::{
    char,
    cow::CowBuilder,
    cursor::Cursor,
    diag::{DiagnosticKind, Expected},
    grammar::{recover, tag, token_char},
    Diagnostic, Receiver, Span, Token,
};

use super::{
    trivia::{self, trailing_line},
    try_token_char,
};

#[derive(Clone, Debug, Default)]
#[cfg_attr(test, derive(serde::Serialize), serde(rename_all = "lowercase"))]
pub(crate) struct Document<'s> {
    explicit: bool,
    version: Option<Cow<'s, str>>,
    tags: BTreeMap<Cow<'s, str>, Cow<'s, str>>,
}

pub(super) fn prefix<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    prev_terminated: bool,
) -> Result<(Document<'s>, Span), Diagnostic> {
    try_token_char(
        cursor,
        receiver,
        Token::ByteOrderMark,
        char::BYTE_ORDER_MARK,
    )?;

    trivia::comment_lines(cursor, receiver)?;

    let mut document = Document::default();
    let start = cursor.location();

    let have_directives = cursor.is_char(char::DIRECTIVE)?;
    if have_directives {
        if !prev_terminated {
            receiver.diagnostic(Diagnostic::new(
                DiagnosticKind::DirectiveAfterUnterminatedDocument,
                cursor.next_span(),
            ));
        }

        directives(cursor, receiver, &mut document)?;
    }

    if cursor.is_str("---")? {
        let is_start_of_line = cursor.is_start_of_line();
        let followed_by_whitespace =
            matches!(cursor.peek_nth(3)?, None | Some('\r' | '\n' | '\t' | ' '));

        if (is_start_of_line && followed_by_whitespace) || have_directives || !prev_terminated {
            cursor.eat_str("---")?;
            let span = cursor.token();
            receiver.token(Token::DirectivesEnd, span);

            if !is_start_of_line {
                receiver.diagnostic(Diagnostic::new(
                    DiagnosticKind::DirectivesEndNotAtStartOfLine,
                    span,
                ));
            }

            if followed_by_whitespace {
                trailing_line(cursor, receiver)?;
            } else {
                receiver.diagnostic(Diagnostic::new(
                    DiagnosticKind::DirectivesEndNotFollowedByWhitespace,
                    span,
                ));
            }

            document.explicit = true;
        }
    } else if have_directives {
        receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::MissingDirectivesEndAfterDirective,
            cursor.empty_span(),
        ));
    } else if !prev_terminated {
        receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::MissingDirectivesEndAfterUnterminatedDocument,
            cursor.empty_span(),
        ));
    }

    let span = cursor.span(start);

    trivia::comment_lines(cursor, receiver)?;

    Ok((document, span))
}

pub(super) fn suffix(
    cursor: &mut Cursor,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Span, Diagnostic> {
    debug_assert!(cursor.is_token_boundary());

    if !cursor.is_start_of_line()
        || !cursor.is_str("...")?
        || !matches!(cursor.peek_nth(3)?, None | Some('\r' | '\n' | '\t' | ' '))
    {
        return Ok(cursor.empty_span());
    }

    cursor.eat_str("...")?;
    let span = cursor.token();
    receiver.token(Token::DocumentEnd, span);

    trivia::trailing_lines(cursor, receiver)?;
    Ok(span)
}

fn directives<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    document: &mut Document<'s>,
) -> Result<(), Diagnostic> {
    while cursor.is_char(char::DIRECTIVE)? {
        if !cursor.is_start_of_line() {
            receiver.diagnostic(Diagnostic::new(
                DiagnosticKind::DirectiveNotAtStartOfLine,
                cursor.next_span(),
            ));
        }

        token_char(cursor, receiver, Token::Directive, char::DIRECTIVE)?;

        if let Err(diag) = directive(cursor, receiver, document) {
            recover(cursor, receiver, diag, |cursor| {
                Ok(cursor.is(char::r#break)?
                    || (cursor.is_separated() && cursor.is_char(char::COMMENT)?))
            })?;
        }

        trivia::trailing_lines(cursor, receiver)?;
    }

    Ok(())
}

fn directive<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    document: &mut Document<'s>,
) -> Result<(), Diagnostic> {
    debug_assert!(cursor.is_token_boundary());
    if cursor.is(char::non_space)? {
        let mut name = CowBuilder::new(cursor);
        name.push_while(cursor, char::non_space)?;
        let name = name.build();
        let name_span = cursor.token();
        receiver.token(Token::DirectiveName, name_span);

        match name.as_ref() {
            "YAML" => yaml_directive(cursor, receiver, document, name_span)?,
            "TAG" => tag_directive(cursor, receiver, document)?,
            _ => reserved_directive(cursor, receiver, name, name_span)?,
        }

        Ok(())
    } else {
        Err(Diagnostic::expected_token(Token::DirectiveName, cursor))
    }
}

fn yaml_directive<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    document: &mut Document<'s>,
    name_span: Span,
) -> Result<(), Diagnostic> {
    if !trivia::try_separate_in_line(cursor, receiver)? {
        return Err(Diagnostic::expected_token(Token::YamlVersion, cursor));
    }

    let mut version = CowBuilder::new(cursor);
    let (major, minor) = yaml_version(cursor, &mut version)?;
    let version = version.build();
    let version_span = cursor.token();
    receiver.token(Token::YamlVersion, version_span);

    if document.version.is_some() {
        receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::DuplicateYamlDirective,
            cursor.span(name_span.start),
        ));
    } else {
        document.version = Some(version);
    }

    match (major, minor) {
        (1, 1 | 2) => (),
        (1, _) => receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::UnknownMinorVersion,
            version_span,
        )),
        (_, _) => receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::UnknownMajorVersion,
            version_span,
        )),
    }

    while let Some(param_span) = try_directive_param(cursor, receiver)? {
        receiver.diagnostic(Diagnostic::new(
            DiagnosticKind::UnexpectedDiagnosticParameter,
            param_span,
        ))
    }

    Ok(())
}

fn yaml_version<'s>(
    cursor: &mut Cursor<'s>,
    value: &mut CowBuilder<'s>,
) -> Result<(u32, u32), Diagnostic> {
    let minor = yaml_version_part(cursor, value)?;

    if !cursor.is_char('.')? {
        return Err(Diagnostic::expected(Expected::Char('.'), cursor));
    }
    value.push(cursor);

    let major = yaml_version_part(cursor, value)?;

    if cursor.is(char::non_space)? {
        return Err(Diagnostic::expected(Expected::DecimalDigit, cursor));
    }

    Ok((minor, major))
}

fn yaml_version_part<'s>(
    cursor: &mut Cursor<'s>,
    value: &mut CowBuilder<'s>,
) -> Result<u32, Diagnostic> {
    let start = cursor.location();
    let value_start = value.len();
    if !cursor.is(char::dec_digit)? {
        return Err(Diagnostic::expected(Expected::DecimalDigit, cursor));
    }
    value.push_while(cursor, char::dec_digit)?;

    value.as_ref()[value_start..].parse::<u32>().map_err(|_| {
        Diagnostic::new(
            DiagnosticKind::VersionOverflow(value.as_ref().to_owned().into()),
            cursor.span(start),
        )
    })
}

fn tag_directive<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    document: &mut Document<'s>,
) -> Result<(), Diagnostic> {
    trivia::separate_in_line(cursor, receiver)?;

    let handle_start = cursor.location();
    let handle = tag::handle(cursor, receiver)?;
    let handle_span = cursor.span(handle_start);

    trivia::separate_in_line(cursor, receiver)?;
    let prefix = tag::prefix(cursor, receiver)?;

    match document.tags.entry(handle) {
        btree_map::Entry::Vacant(entry) => {
            entry.insert(prefix);
        }
        btree_map::Entry::Occupied(entry) => {
            receiver.diagnostic(Diagnostic::new(
                DiagnosticKind::DuplicateTagDirective(entry.key().as_ref().to_owned().into()),
                handle_span,
            ));
        }
    }

    Ok(())
}

fn reserved_directive<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    name: Cow<'s, str>,
    name_span: Span,
) -> Result<(), Diagnostic> {
    receiver.diagnostic(Diagnostic::new(
        DiagnosticKind::UnknownDirective(name.into_owned().into()),
        name_span,
    ));

    while try_directive_param(cursor, receiver)?.is_some() {}

    Ok(())
}

fn try_directive_param(
    cursor: &mut Cursor<'_>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Option<Span>, Diagnostic> {
    if trivia::try_separate_in_line(cursor, receiver)?
        && cursor.is(char::non_space)?
        && !cursor.is_char(char::COMMENT)?
    {
        cursor.eat_while(char::non_space)?;
        let span = cursor.token();
        receiver.token(Token::DirectiveParameter, span);
        Ok(Some(span))
    } else {
        Ok(None)
    }
}

impl<'s> Document<'s> {
    pub fn explicit(&self) -> bool {
        self.explicit
    }

    pub fn version(&self) -> Option<&Cow<'s, str>> {
        self.version.as_ref()
    }
}
