use swamp_script_semantic::{Signature, StructTypeRef, Type};
use swamp_script_source_map::SourceMap;

pub fn span(name: &str, class: &str) -> String {
    format!(r#"  <span class="{}">{}</span>"#, class, name).to_string()
}

pub fn signature(function_type_signature: &Signature) -> String {
    let mut s = "(".to_string();
    for f in &function_type_signature.parameters {
        s += &span(&f.name, "parameter_name");
        s += &span(": ", "separator");
        s += &span(&f.resolved_type.to_string(), "type_name");
    }
    s += ")";

    if let Type::Unit = *function_type_signature.return_type {
        s += "->";
        s += &span(
            &function_type_signature.return_type.to_string(),
            "type_name",
        );
    }

    s
}

pub fn code(inner: &str) -> String {
    format!(
        "<code class=\"language-swamp\" data-lang=\"swamp\">\n{}\n</code>",
        inner
    )
}

pub fn generate_html_doc(_struct_ref: &StructTypeRef, _source_map: &SourceMap) -> String {
    todo!()
}
