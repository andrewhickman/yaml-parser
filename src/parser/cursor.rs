use crate::parser::decode::Stream;

pub struct Cursor<S> {
    stream: S,
}

impl<'s, S> Cursor<S> where S: Stream<'s> {

}
