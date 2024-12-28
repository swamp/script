use eira::Kind;
use std::io::stderr;
use swamp_script_error_report::Report;
use swamp_script_semantic::Span;
use swamp_script_source_map::SourceMap;

#[test]
fn standard() {
    let report = Report::build(
        Kind::Error,
        293,
        "Illegal type parameter",
        &Span {
            file_id: 0,
            offset: 0,
            length: 5,
        },
    )
    .build();

    let mut s = SourceMap::new("tests/fixtures/".as_ref());

    s.add_manual(
        0,
        "test.swamp".as_ref(),
        r"hello, world!
     asdfjisafd",
    );

    report.print(&s, stderr()).expect("print should work");
}