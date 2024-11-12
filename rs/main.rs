use kdl::{KdlDocument, KdlNode, KdlValue};
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::fmt;

struct PrintState {
    indent: usize,
    str: String,
}

impl PrintState {
    fn new() -> Self {
        Self {
            indent: 0,
            str: String::new(),
        }
    }

    fn append(&mut self, fmt: fmt::Arguments) {
        self.str.push_str(&format!("{:?}", fmt));
    }

    fn append_array<T>(&mut self, name: &str, items: &[T], formatter: impl Fn(&T) -> String) {
        if items.is_empty() {
            self.append(format_args!("{}: []", name));
        } else {
            self.append(format_args!("{}:", name));
            for item in items {
                self.append(format_args!("  - {}", formatter(item)));
            }
        }
    }
}

fn print_ast(doc: &KdlDocument, state: &mut PrintState) {
    if state.indent == 0 {
        state.append(format_args!("--- !ruby/object:KDL::Document\n"));
        state.append(format_args!("nodes:"));
    }
    if doc.nodes().is_empty() {
        state.append(format_args!(" []\n"));
    } else {
        state.append(format_args!("\n"));
        for node in doc.nodes() {
            print_node(node, state);
        }
    }
}

fn print_value(value: &KdlValue, state: &mut PrintState) {
    let indent_str = "  ".repeat(state.indent + 2);
    match value {
        KdlValue::String(s) => state.append(format_args!(
            "!ruby/object:KDL::Value::String\n{}value: {:?}\n",
            indent_str, s
        )),
        KdlValue::Base2(n) => state.append(format_args!(
            "!ruby/object:KDL::Value::Number\n{}value: {}\n",
            indent_str, n
        )),
        KdlValue::Base8(n) => state.append(format_args!(
            "!ruby/object:KDL::Value::Number\n{}value: {}\n",
            indent_str, n
        )),
        KdlValue::Base10(n) => state.append(format_args!(
            "!ruby/object:KDL::Value::Number\n{}value: {}\n",
            indent_str, n
        )),
        KdlValue::Base10Float(n) => state.append(format_args!(
            "!ruby/object:KDL::Value::Number\n{}value: {}\n",
            indent_str, n
        )),
        KdlValue::RawString(s) => state.append(format_args!(
            "!ruby/object:KDL::Value::String\n{}value: {:?}\n",
            indent_str, s
        )),
        KdlValue::Base16(n) => state.append(format_args!(
            "!ruby/object:KDL::Value::Number\n{}value: {}\n",
            indent_str, n
        )),
        KdlValue::Bool(b) => state.append(format_args!(
            "!ruby/object:KDL::Value::Bool\n{}value: {}\n",
            indent_str, b
        )),
        KdlValue::Null => state.append(format_args!("{}\n", indent_str)),
    }
}

fn print_node(node: &KdlNode, state: &mut PrintState) {
    let indent_str = "  ".repeat(state.indent);
    state.append(format_args!("{}- !ruby/object:KDL::Node\n", indent_str));
    state.append(format_args!(
        "{}  name: {:?}\n",
        indent_str,
        node.name().value()
    ));

    if let Some(ty) = node.ty() {
        state.append(format_args!("{}    type: {}\n", indent_str, ty));
    }
    let mut args = Vec::new();
    let mut props = Vec::new();
    for entry in node.entries() {
        if entry.name().is_some() {
            props.push(entry);
        } else {
            args.push(entry);
        }
    }

    if !args.is_empty() {
        state.append(format_args!("{}  arguments:\n", indent_str));
        for arg in args {
            state.append(format_args!("{}  - ", indent_str));
            print_value(&arg.value(), state);
        }
    } else {
        state.append(format_args!("{}  arguments: []\n", indent_str));
    }

    if !props.is_empty() {
        state.append(format_args!("{}  properties:\n", indent_str));
        props.sort_by(|a, b| a.name().unwrap().value().cmp(b.name().unwrap().value()));
        state.indent += 1;
        for prop in props {
            state.append(format_args!("{}    {}: ", indent_str, prop.name().unwrap()));
            print_value(&prop.value(), state);
        }
        state.indent -= 1;
    } else {
        state.append(format_args!("{}  properties: {}\n", indent_str, "{}"));
    }

    if let Some(children) = node.children() {
        state.append(format_args!("{}  children:", indent_str));
        state.indent += 1;
        print_ast(children, state);
        state.indent -= 1;
    } else {
        state.append(format_args!("{}  children: []\n", indent_str));
    }
}

fn main() -> miette::Result<()> {
    let doc_str = std::io::read_to_string(std::io::stdin()).expect("failed to read from stdin");

    let doc = doc_str.parse::<kdl::KdlDocument>()?;

    let mut state = PrintState::new();
    print_ast(&doc, &mut state);
    println!("{}", state.str);
    Ok(())
}

#[test]
fn test_print_ast() {
    let doc_str = "foo { bar baz=2 \"ok\"; }\n";
    let doc: KdlDocument = doc_str.parse().unwrap();
    let mut state = PrintState::new();
    print_ast(&doc, &mut state);
    assert_eq!(
        state.str,
        r#"--- !ruby/object:KDL::Document
nodes:
- !ruby/object:KDL::Node
  name: "foo"
  arguments: []
  properties: {}
  children:
  - !ruby/object:KDL::Node
    name: "bar"
    arguments:
    - !ruby/object:KDL::Value::String
      value: "ok"
    properties:
      baz: !ruby/object:KDL::Value::Number
        value: 2
    children: []
"#
    );
}
