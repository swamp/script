use eira::{FileSpanMessage, Header, Kind, Pos, PosSpan};
use std::io::stderr;

#[test]
fn header_error() {
    let header = Header {
        header_kind: Kind::Error,
        code: 2044,
        message: "Illegal symbol for the type".to_string(),
    };

    header.write(stderr()).expect("could not write header");
}

#[test]
fn file_span_message() {
    let primary_span = PosSpan {
        pos: Pos { x: 2, y: 4 },
        length: 15,
    };
    FileSpanMessage::write(
        "some/relative/path/something.swamp",
        &primary_span,
        stderr(),
    )
    .expect("write failed for file span");
}

#[test]
fn header_and_file() {
    let header = Header {
        header_kind: Kind::Error,
        code: 2044,
        message: "Illegal symbol for the type".to_string(),
    };
    header.write(stderr()).expect("could not write header");

    let primary_span = PosSpan {
        pos: Pos { x: 2, y: 4 },
        length: 15,
    };
    FileSpanMessage::write("some/path/main.swamp", &primary_span, stderr())
        .expect("write failed for file span");
}