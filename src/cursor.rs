use crate::stream::ByteStream;

pub(crate) struct Cursor<'s> {
    stream: ByteStream<'s>,
}
