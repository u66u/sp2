use crate::AstNode;

pub fn generate_qbe(ast: &AstNode) -> String {
    let mut output = String::new();

    match ast {
        AstNode::Program(nodes) => {
            for node in nodes {
                match node {
                    AstNode::FunctionDef { name, params, return_type: _, body } => {
                        output.push_str(&format!("export function w ${name}("));
                        
                        let params: Vec<String> = params.iter()
                            .map(|(name, _)| format!("w %{name}"))
                            .collect();
                        output.push_str(&params.join(", "));
                        output.push_str(") {\n");

                        output.push_str("@start\n");
                        for stmt in body {
                            match stmt {
                                AstNode::LetStatement { name, value } => {
                                    match value.as_ref() {
                                        AstNode::BinaryOperation { left, operator, right } => {
                                            let left = match left.as_ref() {
                                                AstNode::Identifier(id) => format!("%{id}"),
                                                _ => unreachable!("Expected identifier"),
                                            };
                                            let right = match right.as_ref() {
                                                AstNode::Identifier(id) => format!("%{id}"),
                                                _ => unreachable!("Expected identifier"),
                                            };
                                            let op = match operator.as_str() {
                                                "+" => "add",
                                                "-" => "sub",
                                                "*" => "mul",
                                                "/" => "div",
                                                _ => unreachable!("Unsupported operator"),
                                            };
                                            output.push_str(&format!("    %{name} =w {op} {left}, {right}\n"));
                                        },
                                        AstNode::FunctionCall { name: func_name, args } => {
                                            let args: Vec<String> = args.iter()
                                                .map(|arg| match arg {
                                                    AstNode::Identifier(id) => format!("w %{id}"),
                                                    AstNode::IntLiteral(val) => format!("w {val}"),
                                                    _ => unreachable!("Unsupported argument type"),
                                                })
                                                .collect();
                                            output.push_str(&format!("    %{name} =w call ${func_name}({})\n", args.join(", ")));
                                        },
                                        _ => unreachable!("Expected binary operation or function call"),
                                    }
                                },
                                AstNode::ReturnStatement(expr) => {
                                    match expr.as_ref() {
                                        AstNode::Identifier(id) => {
                                            output.push_str(&format!("    ret %{id}\n"));
                                        },
                                        _ => unreachable!("Expected identifier in return statement"),
                                    }
                                },
                                AstNode::ExpressionStatement(expr) => {
                                    match expr.as_ref() {
                                        AstNode::FunctionCall { name, args } => {
                                            let args: Vec<String> = args.iter()
                                                .map(|arg| match arg {
                                                    AstNode::Identifier(id) => format!("w %{id}"),
                                                    AstNode::IntLiteral(val) => format!("w {val}"),
                                                    _ => unreachable!("Unsupported argument type"),
                                                })
                                                .collect();
                                            output.push_str(&format!("    call ${name}({})\n", args.join(", ")));
                                        },
                                        _ => unreachable!("Expected function call in expression statement"),
                                    }
                                },
                                _ => unreachable!("Unexpected statement type"),
                            }
                        }

                        output.push_str("}\n\n");
                    },
                    _ => unreachable!("Unexpected top-level node"),
                }
            }
        },
        _ => unreachable!("Expected Program node"),
    }

    output
}
