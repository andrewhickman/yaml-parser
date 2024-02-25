use alloc::borrow::Cow;

use crate::Encoding;

/// An event encountered while parsing a YAML stream.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Serialize), serde(tag = "event"))]
pub enum Event<'s> {
    /// Emitted at the start of parsing a YAML stream.
    StreamStart {
        /// The encoding of the YAML stream.
        encoding: Encoding,
    },
    /// Emitted at the end of parsing a YAML stream.
    StreamEnd,
    /// Emitted at the start of each document within a YAML stream.
    DocumentStart {
        /// The YAML version of this document, if specified with a `%YAML` directive.
        version: Option<Cow<'s, str>>,
    },
    /// Emitted at the end of each document within a YAML stream.
    DocumentEnd,
    /// Emitted at the start of a mapping node.
    MappingStart {
        /// The style of this mapping node.
        style: CollectionStyle,
        /// The anchor property at this mapping node, if specified.
        anchor: Option<Cow<'s, str>>,
        /// The tag property of this mapping node, if specified.
        tag: Option<Cow<'s, str>>,
    },
    /// Emitted at the end of a mapping node.
    MappingEnd,
    /// Emitted at the start of a sequence node.
    SequenceStart {
        /// The style of this sequence node.
        style: CollectionStyle,
        /// The anchor property at this sequence node, if specified.
        anchor: Option<Cow<'s, str>>,
        /// The tag property of this sequence node, if specified.
        tag: Option<Cow<'s, str>>,
    },
    /// Emitted at the end of a sequence node.
    SequenceEnd,
    /// Emitted when encountering an alias node.
    Alias {
        /// The name of the anchor this alias refers to.
        value: Cow<'s, str>,
    },
    /// Emitted when encountering a scalar node.
    Scalar {
        /// The presentation style of this scalar node.
        style: ScalarStyle,
        /// The contents of the scalar node.
        value: Cow<'s, str>,
        /// The anchor property at this scalar node, if specified.
        anchor: Option<Cow<'s, str>>,
        /// The tag property of this scalar node, if specified.
        tag: Option<Cow<'s, str>>,
    },
}

/// The presentation style of a sequence or mapping node.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize), serde(rename_all = "lowercase"))]
pub enum CollectionStyle {
    /// A block-style collection:
    ///
    /// ```yaml
    /// key: value
    /// ```
    Block,
    /// A flow-style collection:
    ///
    /// ```yaml
    /// { key: value }
    /// ```
    Flow,
}

/// The presentation style of a scalar node.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize), serde(rename_all = "lowercase"))]
pub enum ScalarStyle {
    /// A plain scalar: `value`.
    Plain,
    /// A single quoted scalar: `'value'`.
    SingleQuoted,
    /// A double quoted scalar: `"value"`.
    DoubleQuoted,
    /// A literal scalar:
    ///
    /// ```yaml
    /// |
    /// value
    /// ```
    Literal,
    /// A folded scalar:
    ///
    /// ```yaml
    /// >
    /// value
    /// ```
    Folded,
}
