use swamp_script_parser::{AstParser, UNKNOWN_FILE_ID};

#[test]
fn test() {
    let parser = AstParser::new(UNKNOWN_FILE_ID);
    parser.parse
}