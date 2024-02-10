//! A pure-rust, safe, YAML parser.
#![no_std]
#![warn(missing_debug_implementations, missing_docs)]
#![deny(unsafe_code)]
#![doc(html_root_url = "https://docs.rs/yaml-parser/0.1.0/")]

extern crate alloc;

mod char;
mod cursor;
mod error;
mod event;
mod grammar;
mod parser;
mod stream;

pub use self::{
    error::Error,
    event::{CollectionStyle, Event, ScalarStyle},
    parser::{Location, Parser, Span, Token},
    stream::Encoding,
};
