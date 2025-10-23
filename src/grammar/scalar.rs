use alloc::borrow::Cow;

use crate::{cursor::Cursor, grammar::Indent, Diagnostic, Receiver};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Chomping {
    Strip,
    Clip,
    Keep,
}

pub(super) fn literal<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    indent: Indent,
) -> Result<Cow<'s, str>, Diagnostic> {
    todo!()
}

pub(super) fn plain<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    indent: Indent,
) -> Result<Cow<'s, str>, Diagnostic> {
    todo!()
}

pub(super) fn single_quoted<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    indent: Indent,
) -> Result<Cow<'s, str>, Diagnostic> {
    todo!()
}

pub(super) fn double_quoted<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
    indent: Indent,
) -> Result<Cow<'s, str>, Diagnostic> {
    todo!()
}
