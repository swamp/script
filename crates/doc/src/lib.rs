use swamp_script_semantic::{FunctionTypeSignature, ResolvedStructTypeRef, ResolvedType};
use swamp_script_source_map::SourceMap;

pub fn span(name: &str, class: &str) -> String {
    format!(r#"  <span class="{}">{}</span>"#, class, name).to_string()
}

pub fn signature(function_type_signature: &FunctionTypeSignature) -> String {
    let mut s = "(".to_string();
    for f in &function_type_signature.parameters {
        s += &span(&f.name, "parameter_name");
        s += &span(": ", "separator");
        s += &span(&*f.resolved_type.as_ref().unwrap().to_string(), "type_name");
    }
    s += ")";

    if ResolvedType::Unit != *function_type_signature.return_type {
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

pub fn generate_html_doc(struct_ref: &ResolvedStructTypeRef, source_map: &SourceMap) -> String {
    let mut s = "".to_string();

    for (name, func) in &struct_ref.borrow().functions {
        s += &*format!("## {}\n {}", name, code(&*signature(func.signature())));
        if let Some(markdown_text) = func.as_ref().node().markdown_doc {
            s += source_map.get_span_source(
                markdown_text.file_id,
                markdown_text.offset as usize,
                markdown_text.length as usize,
            );
        }
    }

    s
}
