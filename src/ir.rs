use crate::AstNode;
use std::collections::HashMap;

pub fn generate_qbe(ast: &AstNode) -> String {
    let mut output = String::new();
    let mut global_vars = HashMap::new();
    let mut temp_counter = 0;

    output.push_str("function w $print(w %x) {\n");
    output.push_str("@start\n");
    output.push_str("    call $printf(l $fmt, w %x)\n");
    output.push_str("    ret 0\n");
    output.push_str("}\n\n");

    output.push_str("data $fmt = { b \"%d\\n\", b 0 }\n\n");

    if let AstNode::Program(nodes) = ast {
        for node in nodes {
            match node {
                AstNode::FunctionDef {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    generate_function(
                        name,
                        params,
                        return_type,
                        body,
                        &mut output,
                        &mut temp_counter,
                    );
                }
                AstNode::LetStatement { name, value } => {
                    global_vars.insert(name.clone(), value.clone());
                }
                _ => unreachable!("Unexpected top-level node: {:?}", node),
            }
        }
    }

    for (name, value) in global_vars {
        generate_global_variable(&name, value, &mut output, &mut temp_counter);
    }

    output
}

fn generate_function(
    name: &str,
    params: &[(String, String)],
    return_type: &str,
    body: &[AstNode],
    output: &mut String,
    temp_counter: &mut i32,
) {
    let qbe_return_type = convert_type_str(return_type);
    output.push_str(&format!("export function {} ${}(", qbe_return_type, name));

    let params: Vec<String> = params
        .iter()
        .map(|(name, typ)| format!("{} %{}", convert_type_str(typ), name))
        .collect();
    output.push_str(&params.join(", "));
    output.push_str(") {\n@start\n");

    for stmt in body {
        generate_statement(stmt, output, temp_counter);
    }

    if !body
        .last()
        .map_or(false, |stmt| matches!(stmt, AstNode::ReturnStatement(_)))
    {
        output.push_str("    ret 0\n");
    }

    output.push_str("}\n\n");
}

fn generate_global_variable(
    name: &str,
    value: Box<AstNode>,
    output: &mut String,
    temp_counter: &mut i32,
) {
    output.push_str(&format!("data ${} = {{ w 0 }}\n\n", name));
    output.push_str(&format!("function w $init_{}() {{\n", name));
    output.push_str("@start\n");
    let temp = generate_expression(&value, output, temp_counter);
    output.push_str(&format!("    storew {}, ${}\n", temp, name));
    output.push_str("    ret 0\n");
    output.push_str("}\n\n");
}

fn generate_statement(stmt: &AstNode, output: &mut String, temp_counter: &mut i32) {
    match stmt {
        AstNode::LetStatement { name, value } => {
            let temp = generate_expression(value, output, temp_counter);
            output.push_str(&format!("    %{} =w copy {}\n", name, temp));
        }
        AstNode::ReturnStatement(expr) => {
            let temp = generate_expression(expr, output, temp_counter);
            output.push_str(&format!("    ret {}\n", temp));
        }
        AstNode::ExpressionStatement(expr) => {
            generate_expression(expr, output, temp_counter);
        }
        _ => unreachable!("Unexpected statement type: {:?}", stmt),
    }
}

fn generate_expression(expr: &AstNode, output: &mut String, temp_counter: &mut i32) -> String {
    match expr {
        AstNode::Identifier(id) => format!("%{}", id),
        AstNode::IntLiteral(val) => val.to_string(),
        AstNode::BinaryOperation {
            left,
            operator,
            right,
        } => {
            let left_temp = generate_expression(left, output, temp_counter);
            let right_temp = generate_expression(right, output, temp_counter);
            let op = match operator.as_str() {
                "+" => "add",
                "-" => "sub",
                "*" => "mul",
                "/" => "div",
                _ => unreachable!("Unsupported operator"),
            };
            *temp_counter += 1;
            let result_temp = format!("%t{}", temp_counter);
            output.push_str(&format!(
                "    {} =w {} {}, {}\n",
                result_temp, op, left_temp, right_temp
            ));
            result_temp
        }
        AstNode::FunctionCall { name, args } => {
            let mut arg_temps = Vec::new();
            for arg in args {
                arg_temps.push(generate_expression(arg, output, temp_counter));
            }
            *temp_counter += 1;
            let result_temp = format!("%t{}", temp_counter);
            output.push_str(&format!(
                "    {} =w call ${}({})\n",
                result_temp,
                name,
                arg_temps.join(", ")
            ));
            result_temp
        }
        _ => unreachable!("Unexpected expression type: {:?}", expr),
    }
}

fn convert_type_str(typ: &str) -> &'static str {
    match typ {
        "Int" => "w",
        "String" => "l",
        "None" => "w",
        _ => unreachable!("Unsupported type: {}", typ),
    }
}
