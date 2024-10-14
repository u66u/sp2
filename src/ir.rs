use crate::AstNode;

pub fn generate_qbe(ast: &AstNode) -> String {
    let mut output = String::new();

    output.push_str("function $print(w %x) {\n");
    output.push_str("@start\n");
    output.push_str("    call $printf(l $fmt, w %x)\n");
    output.push_str("    ret\n");
    output.push_str("}\n\n");

    output.push_str("data $fmt = { b \"%d\\n\", b 0 }\n\n");

    match ast {
        AstNode::Program(nodes) => {
            for node in nodes {
                match node {
                    AstNode::FunctionDef {
                        name,
                        params,
                        return_type,
                        body,
                    } => {
                        output.push_str(&format!("export function w ${name}("));

                        let params: Vec<String> = params
                            .iter()
                            .map(|(name, _)| format!("w %{name}"))
                            .collect();
                        output.push_str(&params.join(", "));
                        output.push_str(") {\n");

                        output.push_str("@start\n");
                        for stmt in body {
                            generate_statement(stmt, &mut output);
                        }

                        output.push_str("}\n\n");
                    }
                    AstNode::LetStatement { name, value } => {
                        output.push_str(&format!("export data ${name} = {{ w ",));
                        generate_expression(value, &mut output);
                        output.push_str(" }\n\n");
                    }
                    _ => unreachable!("Unexpected top-level node: {:?}", node),
                }
            }
        }
        _ => unreachable!("Expected Program node"),
    }

    output
}

fn generate_statement(stmt: &AstNode, output: &mut String) {
    match stmt {
        AstNode::LetStatement { name, value } => {
            output.push_str(&format!("    %{name} =w "));
            generate_expression(value, output);
            output.push_str("\n");
        }
        AstNode::ReturnStatement(expr) => {
            output.push_str("    ret ");
            generate_expression(expr, output);
            output.push_str("\n");
        }
        AstNode::ExpressionStatement(expr) => {
            generate_expression(expr, output);
            output.push_str("\n");
        }
        _ => unreachable!("Unexpected statement type: {:?}", stmt),
    }
}

fn generate_expression(expr: &AstNode, output: &mut String) {
    match expr {
        AstNode::Identifier(id) => output.push_str(&format!("%{id}")),
        AstNode::IntLiteral(val) => output.push_str(&val.to_string()),
        AstNode::BinaryOperation {
            left,
            operator,
            right,
        } => {
            let op = match operator.as_str() {
                "+" => "add",
                "-" => "sub",
                "*" => "mul",
                "/" => "div",
                _ => unreachable!("Unsupported operator"),
            };
            output.push_str(&format!("{op} "));
            generate_expression(left, output);
            output.push_str(", ");
            generate_expression(right, output);
        }
        AstNode::FunctionCall { name, args } => {
            output.push_str(&format!("call ${name}("));
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    output.push_str(", ");
                }
                generate_expression(arg, output);
            }
            output.push_str(")");
        }
        _ => unreachable!("Unexpected expression type: {:?}", expr),
    }
}
