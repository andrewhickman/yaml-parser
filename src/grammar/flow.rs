use alloc::borrow::Cow;

use crate::{cursor::Cursor, grammar::Indent, Diagnostic, Receiver, Span};

pub(super) fn sequence_start<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Span, Diagnostic> {
    todo!()
}

pub(super) fn mapping_start<'s>(
    cursor: &mut Cursor<'s>,
    receiver: &mut (impl Receiver + ?Sized),
) -> Result<Span, Diagnostic> {
    todo!()
}
