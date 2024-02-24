use core::{
    fmt,
    ops::{Index, Range},
};

use alloc::borrow::Cow;

use crate::{cursor::Cursor, Diagnostic};

pub enum CowBuilder<'s> {
    Borrowed {
        stream: &'s str,
        range: Range<usize>,
    },
    Owned {
        buffer: String,
    },
}

impl<'s> CowBuilder<'s> {
    pub fn new(cursor: &Cursor<'s>) -> Self {
        match cursor.as_str() {
            Some(stream) => CowBuilder::Borrowed {
                stream,
                range: cursor.index()..cursor.index(),
            },
            None => todo!(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            CowBuilder::Borrowed { range, .. } => range.len(),
            CowBuilder::Owned { buffer } => buffer.len(),
        }
    }

    pub fn push(&mut self, cursor: &mut Cursor<'s>) {
        match self {
            CowBuilder::Borrowed { range, .. } if range.end == cursor.index() => {
                cursor.bump();
                range.end = cursor.index();
            }
            _ => self.push_char(cursor.bump()),
        }
    }

    pub fn push_char(&mut self, ch: char) {
        match self {
            CowBuilder::Borrowed { stream, range } => {
                let mut buffer = String::with_capacity((range.len() * 2).min(8));
                buffer.push_str(&stream[..range.len()]);
                buffer.push(ch);
                *self = CowBuilder::Owned { buffer };
            }
            CowBuilder::Owned { buffer } => buffer.push(ch),
        }
    }

    pub fn push_while(
        &mut self,
        cursor: &mut Cursor<'s>,
        pred: impl Fn(char) -> bool + Clone,
    ) -> Result<(), Diagnostic> {
        match self {
            CowBuilder::Borrowed { range, .. } if range.end == cursor.index() => {
                cursor.eat_while(pred)?;
                range.end = cursor.index();
            }
            _ => {
                while cursor.is(pred.clone())? {
                    self.push_char(cursor.bump());
                }
            }
        }

        Ok(())
    }

    pub fn build(self) -> Cow<'s, str> {
        match self {
            CowBuilder::Borrowed { stream, range } => Cow::Borrowed(&stream[..range.len()]),
            CowBuilder::Owned { buffer } => Cow::Owned(buffer),
        }
    }
}

impl<'s> fmt::Debug for CowBuilder<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<'s> AsRef<str> for CowBuilder<'s> {
    fn as_ref(&self) -> &str {
        match self {
            Self::Borrowed { stream, range } => stream[..range.len()].as_ref(),
            Self::Owned { buffer } => buffer.as_ref(),
        }
    }
}
