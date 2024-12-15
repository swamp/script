use std::io;
use std::path::Path;
use swamp_script_source_map::SourceMap;

#[test_log::test]
fn test() -> io::Result<()> {
    let mut sources = SourceMap::new(Path::new("tests/fixtures"));

    let (x, _script) = sources.read_file_relative("first/world.swamp")?;
    assert_eq!(x, 1);

    let rocket_offset = 83;

    let text = sources.get_span_source(1, rocket_offset, 4);
    assert_eq!(text, "🚀");

    let (row, col) = sources.get_span_location_utf8(1, rocket_offset);
    assert_eq!((row, col), (5, 22));

    let after_text = sources.get_span_source(1, rocket_offset + 4, 3);
    assert_eq!(after_text, "{a}");

    let (row_after, col_after) = sources.get_span_location_utf8(1, rocket_offset + 4);
    assert_eq!((row_after, col_after), (5, 23));

    Ok(())
}
