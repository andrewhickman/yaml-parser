use std::{fs, path::PathBuf};

use serde::Deserialize;
use serde_yaml;
use yaml_parser::{Event, Receiver, Span, Token};

#[derive(Debug, Deserialize)]
struct Case {
    name: String,
    tags: String,
    #[serde(default)]
    fail: bool,
    #[serde(default)]
    skip: bool,
    yaml: String,
    tree: String,
    #[serde(default)]
    json: Option<String>,
    #[serde(default)]
    dump: String,
}

#[derive(Default)]
struct TestReceiver {
    tokens: Vec<(Token, Span)>,
}

impl Receiver for TestReceiver {
    fn event(&mut self, _: Event, _: Span) {}

    fn token(&mut self, token: Token, span: Span) {
        self.tokens.push((token, span))
    }
}

macro_rules! case {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            case($file);
        }
    };
}

fn case(file: &str) {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("yaml-test-suite")
        .join("src")
        .join(file);
    let yaml = fs::read(path).unwrap();
    let cases: Vec<Case> = serde_yaml::from_slice(&yaml).unwrap();

    for case in cases {
        if case.skip {
            // todo
            continue;
        }

        let mut receiver = TestReceiver::default();
        if case.fail {
            assert!(yaml_parser::parse(&mut receiver, &case.yaml).is_err());
        } else {
            assert!(yaml_parser::parse(&mut receiver, &case.yaml).is_ok());
        }
    }
}

include!("cases/mod.rs");
