use std::{fs, path::PathBuf};

use yaml_parser::{Event, Receiver, Span, Token};

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
            case($file, true);
        }
    };
    ($name:ident, $file:literal, fail: true) => {
        #[test]
        fn $name() {
            case($file, false);
        }
    };
}

fn case(name: &str, success: bool) {
    let _ = tracing_subscriber::fmt::try_init();

    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("yaml-test-suite")
        .join(name)
        .join("in.yaml");
    let yaml = fs::read_to_string(path).unwrap();

    let mut receiver = TestReceiver::default();
    if success {
        assert!(yaml_parser::parse(&mut receiver, &yaml).is_ok());
    } else {
        assert!(yaml_parser::parse(&mut receiver, &yaml).is_err());
    }
}

include!("suite/cases.rs");
