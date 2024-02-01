use core::{
    char::{DecodeUtf16, DecodeUtf16Error},
    ops::Range,
    slice::ChunksExact,
    str::Chars,
};

enum Encoding {
    Utf8,
    Utf16,
    Utf32,
}

#[derive(Debug, Clone)]
struct DecodeError {
    kind: DecodeErrorKind,
    range: Range<usize>,
}

#[derive(Debug, Copy, Clone)]
enum DecodeErrorKind {
    InvalidUtf8,
    InvalidUtf16,
    InvalidUtf32,
    InvalidLength,
}

trait Reader<'t>: Iterator<Item = Result<char, DecodeError>> + Clone {
    /// The offset into the original text.
    fn index(&self) -> usize;

    /// Get a reference to the remainder of the text, if supported
    fn as_str(&self) -> Option<&'t str> {
        None
    }
}

#[derive(Debug, Clone)]
struct StringReader<'t> {
    text: &'t str,
    iter: Chars<'t>,
}

#[derive(Debug, Clone)]
enum BytesReader<'t> {
    Error(DecodeError),
    Utf8(StringReader<'t>),
    Utf16Le {
        index: usize,
        iter: DecodeUtf16<U16LeIter<'t>>,
    },
    Utf16Be {
        index: usize,
        iter: DecodeUtf16<U16BeIter<'t>>,
    },
    Utf32Le {
        index: usize,
        iter: U32LeIter<'t>,
    },
    Utf32Be {
        index: usize,
        iter: U32BeIter<'t>,
    },
}

macro_rules! int_iter {
    ($name:ident, $int:ty, $len:expr, $from:path) => {
        #[derive(Debug, Clone)]
        struct $name<'t> {
            chunks: ChunksExact<'t, u8>,
        }

        impl<'t> $name<'t> {
            fn new(text: &'t [u8]) -> Result<Self, DecodeError> {
                let rem = text.len() % $len;
                if rem == 0 {
                    Ok($name {
                        chunks: text.chunks_exact($len),
                    })
                } else {
                    let range = (text.len() - rem)..text.len();
                    Err(DecodeError {
                        range,
                        kind: DecodeErrorKind::InvalidLength,
                    })
                }
            }
        }

        impl<'t> Iterator for $name<'t> {
            type Item = $int;

            fn next(&mut self) -> Option<Self::Item> {
                match self.chunks.next() {
                    Some(slice) => Some($from(slice.try_into().unwrap())),
                    None => None,
                }
            }
        }
    };
}

int_iter!(U16LeIter, u16, 2, u16::from_le_bytes);
int_iter!(U16BeIter, u16, 2, u16::from_be_bytes);
int_iter!(U32LeIter, u32, 2, u32::from_le_bytes);
int_iter!(U32BeIter, u32, 2, u32::from_be_bytes);

impl<'t> Reader<'t> for StringReader<'t> {
    fn index(&self) -> usize {
        self.text.len() - self.iter.as_str().len()
    }

    fn as_str(&self) -> Option<&'t str> {
        Some(self.iter.as_str())
    }
}

impl<'t> Iterator for StringReader<'t> {
    type Item = Result<char, DecodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(Ok)
    }
}

impl<'t> Reader<'t> for BytesReader<'t> {
    fn index(&self) -> usize {
        match self {
            BytesReader::Error(error)  => error.range.start,
            BytesReader::Utf8(reader) => reader.index(),
            BytesReader::Utf16Le { index, .. }
            | BytesReader::Utf16Be { index, .. }
            | BytesReader::Utf32Le { index, .. }
            | BytesReader::Utf32Be { index, .. } => *index,
        }
    }

    fn as_str(&self) -> Option<&'t str> {
        match self {
            BytesReader::Utf8(reader) => reader.as_str(),
            _ => None,
        }
    }
}

impl<'t> Iterator for BytesReader<'t> {
    type Item = Result<char, DecodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            BytesReader::Error(error) => Some(Err(error.clone())),
            BytesReader::Utf8(reader) => reader.next(),
            BytesReader::Utf16Le { iter, index } => next_utf16(iter, index),
            BytesReader::Utf16Be { iter, index } => next_utf16(iter, index),
            BytesReader::Utf32Le { iter, index } => next_utf32(iter, index),
            BytesReader::Utf32Be { iter, index } => next_utf32(iter, index),
        }
    }
}

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
            kind: DecodeErrorKind::InvalidUtf16,
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
                kind: DecodeErrorKind::InvalidUtf32,
            })),
        },
        None => None,
    }
}
