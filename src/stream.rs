use core::{
    char::{DecodeUtf16, DecodeUtf16Error}, fmt::{self, Write}, ops::Range, slice::ChunksExact, str::Chars
};

use crate::{Location, Span};

/// The encoding of a YAML stream.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Encoding {
    /// UTF-8 encoding.
    Utf8,
    /// Big-endian UTF-16 encoding.
    Utf16Be,
    /// Little-endian UTF-16 encoding.
    Utf16Le,
    /// Big-endian UTF-32 encoding.
    Utf32Be,
    /// Little-endian UTF-32 encoding.
    Utf32Le,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum DecodeError {
    InvalidUtf8,
    InvalidUtf16,
    InvalidUtf32,
    InvalidLength,
}

#[derive(Clone)]
pub(crate) struct Stream<'s> {
    kind: StreamKind<'s>,
}

#[derive(Clone)]
enum StreamKind<'s> {
    Utf8 {
        stream: &'s str,
        iter: Chars<'s>,
    },
    Utf16Be {
        index: usize,
        iter: DecodeUtf16<U16BeIter<'s>>,
    },
    Utf16Le {
        index: usize,
        iter: DecodeUtf16<U16LeIter<'s>>,
    },
    Utf32Be {
        index: usize,
        iter: U32BeIter<'s>,
    },
    Utf32Le {
        index: usize,
        iter: U32LeIter<'s>,
    },
}

impl<'s> Stream<'s> {
    pub(crate) fn from_str(stream: &'s str) -> Self {
        Stream {
            kind: StreamKind::Utf8 {
                stream,
                iter: stream.chars(),
            },
        }
    }

    pub(crate) fn from_slice(stream: &'s [u8]) -> Result<Self, (DecodeError, usize)> {
        let kind = match stream {
            [0x00, 0x00, 0xfe, 0xff, ..] | [0x00, 0x00, 0x00, _, ..] => StreamKind::Utf32Be {
                index: 0,
                iter: U32BeIter::new(stream)?,
            },
            [0xff, 0xfe, 0x00, 0x00, ..] | [_, 0x00, 0x00, 0x00, ..] => StreamKind::Utf32Le {
                index: 0,
                iter: U32LeIter::new(stream)?,
            },
            [0xfe, 0xff, ..] | [0x00, _, ..] => StreamKind::Utf16Be {
                index: 0,
                iter: char::decode_utf16(U16BeIter::new(stream)?),
            },
            [0xff, 0xfe, ..] | [_, 0x00, ..] => StreamKind::Utf16Le {
                index: 0,
                iter: char::decode_utf16(U16LeIter::new(stream)?),
            },
            _ => match core::str::from_utf8(stream) {
                Ok(stream) => StreamKind::Utf8 {
                    stream,
                    iter: stream.chars(),
                },
                Err(err) => return Err((DecodeError::InvalidLength, err.valid_up_to()))
            },
        };

        Ok(Stream { kind })
    }

    pub(crate) fn encoding(&self) -> Encoding {
        match self.kind {
            StreamKind::Utf8 { .. } => Encoding::Utf8,
            StreamKind::Utf16Be { .. } => Encoding::Utf16Be,
            StreamKind::Utf16Le { .. } => Encoding::Utf16Le,
            StreamKind::Utf32Be { .. } => Encoding::Utf32Be,
            StreamKind::Utf32Le { .. } => Encoding::Utf32Le,
        }
    }

    pub(crate) fn index(&self) -> usize {
        match &self.kind {
            StreamKind::Utf8 { stream: text, iter } => text.len() - iter.as_str().len(),
            StreamKind::Utf16Be { index, .. }
            | StreamKind::Utf16Le { index, .. }
            | StreamKind::Utf32Be { index, .. }
            | StreamKind::Utf32Le { index, .. } => *index,
        }
    }

    pub(crate) fn prev(&self) -> Option<char> {
        let index = self.index();
        match &self.kind {
            StreamKind::Utf8 { stream, iter } => todo!(),
            StreamKind::Utf16Be { index, iter } => todo!(),
            StreamKind::Utf16Le { index, iter } => todo!(),
            StreamKind::Utf32Be { index, iter } => todo!(),
            StreamKind::Utf32Le { index, iter } => todo!(),
        }
    }

    pub(crate) fn as_str(&self) -> Option<&'s str> {
        match &self.kind {
            StreamKind::Utf8 { iter, .. } => Some(iter.as_str()),
            _ => None,
        }
    }
}

impl<'s> fmt::Debug for Stream<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        let mut iter = self.clone();
        for _ in 0..64 {
            match iter.next() {
                Some(Ok('\'')) => f.write_char('\'')?,
                Some(Ok(ch)) => fmt::Display::fmt(&ch.escape_debug(), f)?,
                Some(Err(_)) => {
                    f.write_char(char::REPLACEMENT_CHARACTER)?;
                    break;
                }
                None => {
                    break;
                }
            }
        }
        if iter.next().is_some() {
            f.write_char('…')?;
        }
        f.write_char('"')
    }
}

impl<'s> Iterator for Stream<'s> {
    type Item = Result<char, DecodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.kind {
            StreamKind::Utf8 { iter, .. } => iter.next().map(Ok),
            StreamKind::Utf16Be { iter, index } => next_utf16(iter, index),
            StreamKind::Utf16Le { iter, index } => next_utf16(iter, index),
            StreamKind::Utf32Be { iter, index } => next_utf32(iter, index),
            StreamKind::Utf32Le { iter, index } => next_utf32(iter, index),
        }
    }
}

macro_rules! int_iter {
    ($name:ident, $int:ty, $len:expr, $from:path) => {
        #[derive(Debug, Clone)]
        struct $name<'s> {
            chunks: ChunksExact<'s, u8>,
        }

        impl<'s> $name<'s> {
            fn new(stream: &'s [u8]) -> Result<Self, (DecodeError, usize)> {
                let rem = stream.len() % $len;
                if rem == 0 {
                    Ok($name {
                        chunks: stream.chunks_exact($len),
                    })
                } else {
                    let index = stream.len() - rem;
                    Err((DecodeError::InvalidLength, index))
                }
            }
        }

        impl<'s> Iterator for $name<'s> {
            type Item = $int;

            fn next(&mut self) -> Option<Self::Item> {
                match self.chunks.next() {
                    Some(chunk) => Some($from(chunk.try_into().unwrap())),
                    None => None,
                }
            }

            fn nth(&mut self, n: usize) -> Option<Self::Item> {
                match self.chunks.nth(n) {
                    Some(chunk) => Some($from(chunk.try_into().unwrap())),
                    None => None,
                }
            }
        }
    };
}

int_iter!(U16BeIter, u16, 2, u16::from_be_bytes);
int_iter!(U16LeIter, u16, 2, u16::from_le_bytes);
int_iter!(U32BeIter, u32, 2, u32::from_be_bytes);
int_iter!(U32LeIter, u32, 2, u32::from_le_bytes);

fn next_utf16(
    iter: &mut impl Iterator<Item = Result<char, DecodeUtf16Error>>,
    index: &mut usize,
) -> Option<Result<char, DecodeError>> {
    match iter.next() {
        Some(Ok(ch)) => {
            *index += ch.len_utf16();
            Some(Ok(ch))
        }
        Some(Err(_)) => Some(Err(DecodeError {
            range: *index..(*index + 2),
            kind: DecodeError::InvalidUtf16,
        })),
        None => None,
    }
}

fn next_utf32(
    iter: &mut impl Iterator<Item = u32>,
    index: &mut usize,
) -> Option<Result<char, DecodeError>> {
    match iter.next() {
        Some(i) => match char::try_from(i) {
            Ok(ch) => {
                *index += 4;
                Some(Ok(ch))
            }
            Err(_) => Some(Err(DecodeError {
                range: *index..(*index + 4),
                kind: DecodeError::InvalidUtf32,
            })),
        },
        None => None,
    }
}

impl DecodeError {
    pub(crate) fn span(&self) -> Span {
        Span {
            start: Location {
                index: self.range.start,
                line: 0,
                column: 0,
            },
            end: Location {
                index: self.range.end,
                line: 0,
                column: 0,
            },
        }
    }

    pub(crate) fn kind(&self) -> DecodeError {
        self.kind
    }
}
