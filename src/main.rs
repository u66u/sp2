use std::fs;

use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sp2.pest"]
struct SP2parser;

#[derive(Debug)]
enum AstNode {
    Program(Vec<AstNode>),
    FunctionDef {
        name: String,
        params: Vec<(String, String)>,
        return_type: String,
        body: Vec<AstNode>,
    },
    Block(Vec<AstNode>),
    LetStatement {
        name: String,
        value: Box<AstNode>,
    },
    ReturnStatement(Box<AstNode>),
    ExpressionStatement(Box<AstNode>),
    BinaryOperation {
        left: Box<AstNode>,
        operator: String,
        right: Box<AstNode>,
    },
    FunctionCall {
        name: String,
        args: Vec<AstNode>,
    },
    Identifier(String),
    IntLiteral(i64),
    StringLiteral(String),
}

fn parse_program(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::program);
    let nodes: Vec<AstNode> = pair
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::function_def => Some(parse_function_def(pair)),
            Rule::top_level_statement => Some(parse_top_level_statement(pair)),
            Rule::block => Some(parse_block(pair)),
            Rule::EOI => None,
            _ => {
                println!("Unexpected rule in program: {:?}", pair.as_rule());
                None
            }
        })
        .collect();
    AstNode::Program(nodes)
}

fn parse_top_level_statement(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::top_level_statement);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::let_statement => parse_let_statement(inner),
        Rule::function_call => parse_function_call(inner),
        _ => unreachable!(),
    }
}

fn parse_function_def(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::function_def);
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let params = parse_param_list(inner.next().unwrap());
    let return_type = inner.next().unwrap().as_str().to_string();

    let body: Vec<AstNode> = inner
        .filter(|p| p.as_rule() == Rule::statement || p.as_rule() == Rule::block)
        .map(|p| match p.as_rule() {
            Rule::statement => parse_statement(p),
            Rule::block => parse_block(p),
            _ => unreachable!(),
        })
        .collect();

    AstNode::FunctionDef {
        name,
        params,
        return_type,
        body,
    }
}

fn parse_function_call(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::function_call);
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let args = if let Some(arg_list) = inner.next() {
        arg_list.into_inner().map(parse_expression).collect()
    } else {
        Vec::new()
    };
    AstNode::FunctionCall { name, args }
}

fn parse_expression_atom(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::literal => parse_literal(pair),
        Rule::ident => AstNode::Identifier(pair.as_str().to_string()),
        Rule::function_call => parse_function_call(pair),
        Rule::expression => parse_expression(pair),
        _ => unreachable!(),
    }
}

fn parse_param_list(pair: Pair<Rule>) -> Vec<(String, String)> {
    assert_eq!(pair.as_rule(), Rule::param_list);
    pair.into_inner()
        .map(|param| {
            let mut inner = param.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let type_annotation = inner.next().unwrap().as_str().to_string();
            (name, type_annotation)
        })
        .collect()
}

fn parse_block(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::block);
    let statements: Vec<AstNode> = pair
        .into_inner()
        .filter(|p| p.as_rule() == Rule::statement)
        .map(parse_statement)
        .collect();
    AstNode::Block(statements)
}

fn parse_statement(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::statement);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::let_statement => parse_let_statement(inner),
        Rule::return_statement => parse_return_statement(inner),
        Rule::expression_statement => parse_expression_statement(inner),
        _ => unreachable!(),
    }
}

fn parse_let_statement(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::let_statement);
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let value = Box::new(parse_expression(inner.next().unwrap()));
    AstNode::LetStatement { name, value }
}

fn parse_expression_statement(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::expression_statement);
    let expr = pair.into_inner().next().unwrap();
    AstNode::ExpressionStatement(Box::new(parse_expression(expr)))
}

fn parse_expression(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::expression);
    parse_binary_operation(pair.into_inner().next().unwrap())
}

fn parse_return_statement(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::return_statement);
    let expr = pair.into_inner().next().unwrap();
    AstNode::ReturnStatement(Box::new(parse_expression(expr)))
}

fn parse_binary_operation(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::binary_operation);
    let mut pairs = pair.into_inner();
    let mut left = parse_unary_operation(pairs.next().unwrap());

    while let Some(op) = pairs.next() {
        let right = parse_unary_operation(pairs.next().unwrap());
        left = AstNode::BinaryOperation {
            left: Box::new(left),
            operator: op.as_str().to_string(),
            right: Box::new(right),
        };
    }

    left
}

fn parse_unary_operation(pair: Pair<Rule>) -> AstNode {
    parse_expression_atom(pair)
}

fn parse_primary(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::literal => parse_literal(pair),
        Rule::ident => AstNode::Identifier(pair.as_str().to_string()),
        Rule::function_call => parse_function_call(pair),
        Rule::expression => parse_expression(pair),
        _ => unreachable!(),
    }
}

fn parse_literal(pair: Pair<Rule>) -> AstNode {
    assert_eq!(pair.as_rule(), Rule::literal);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::integer => AstNode::IntLiteral(inner.as_str().parse().unwrap()),
        Rule::string => {
            // Remove the surrounding quotes and unescape the string
            let raw_string = inner.as_str();
            let unquoted = &raw_string[1..raw_string.len() - 1];
            let unescaped = unescape(unquoted);
            AstNode::StringLiteral(unescaped)
        }
        _ => unreachable!(),
    }
}

fn unescape(s: &str) -> String {
    let mut unescaped = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => unescaped.push('\n'),
                Some('r') => unescaped.push('\r'),
                Some('t') => unescaped.push('\t'),
                Some('\\') => unescaped.push('\\'),
                Some('\'') => unescaped.push('\''),
                Some('\"') => unescaped.push('\"'),
                Some(c) => {
                    unescaped.push('\\');
                    unescaped.push(c);
                }
                None => unescaped.push('\\'),
            }
        } else {
            unescaped.push(ch);
        }
    }
    unescaped
}

fn main() {
    let input = r#"
fn foo1_2(x: Int, y: Int) -> Int {
    let z = x + y
    return z
}
 
let x = 1
let y = 2


fn bar(s: String) -> String {
    return s + " ."
{}
}
{}
    "#;

    let mut pairs = SP2parser::parse(Rule::program, input).unwrap();
    let ast = parse_program(pairs.next().unwrap());
    println!("{:#?}", ast);
}
