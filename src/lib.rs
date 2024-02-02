//! A pure-rust, safe, YAML parser.
#![no_std]
#![warn(missing_debug_implementations, missing_docs)]
#![deny(unsafe_code)]
#![doc(html_root_url = "https://docs.rs/yaml-parser/0.1.0/")]

extern crate alloc;

mod error;
mod event;
mod parser;

pub use self::{
    error::Error,
    event::{CollectionStyle, Event, ScalarStyle},
    parser::{Encoding, Location, Parser, Span, Token},
};
