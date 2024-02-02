
enum State {
    /// Parsing a stream, looking for directives or document start markers.
    Stream,
    /// Parsing a document, looking for a node or a document end marker.
    Document {
        explicit: bool,
    },
    /// Parsing a node, determining which kind it is.
    Node {
        indent: i32,
    },
    /// We're in a mapping
    Mapping {
    },
    CompactMapping {
    },
    FlowMapping {
    },

    /// We're in a sequence
    Sequence {
    },
    CompactSequence {
    },
    FlowSequence {
    },
    
    /// We're in a sequence
    Scalar,
}

pub fn production<'s, S>(cursor: Cursor<S> /* parser/state/n/c? */) {

}