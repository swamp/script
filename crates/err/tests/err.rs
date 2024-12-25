use swamp_script_error_report::{Kind, Report};
use swamp_script_semantic::Span;
use swamp_script_source_map::SourceMap;

#[test]
fn standard() {
    let report = Report::build(
        Kind::Error,
        293,
        "Illegal type parameter",
        Span {
            file_id: 0,
            offset: 0,
            length: 6,
        },
    )
    .build();

    let mut s = SourceMap::new("tests/fixtures/".as_ref());

    s.add_manual(0, "test.swamp".as_ref(), "hello, world!");

    report.print(&s);
}
