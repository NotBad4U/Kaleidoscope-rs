use crate::lexer::Token::*;
use crate::lexer::*;
use plex::parser;
use uuid::Uuid; // mangle function
                /*
                ==== EBNF Grammar
                program          : [[statement | expression] Delimiter ? ]*;
                statement        : [declaration | definition];
                declaration      : Extern prototype;
                definition       : Def prototype expression;
                prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
                expression       : [primary_expr (Op primary_expr)*];
                primary_expr     : [Ident | Number | call_expr | parenthesis_expr];
                call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
                parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
                */

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mult,
    GreaterThan,
    LowerThan,
    Eq,
    Equiv,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Number(f64),
    Variable(String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    ForLoop(
        String,
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Box<Expression>,
    ),
    WhileLoop(Box<Expression>, Box<Expression>),
    VarIn(String, Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub proto: Prototype,
    pub body: Option<Expression>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub stmts: Vec<ASTNode>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode {
    Extern(Prototype),
    Function(Function),
    Expression(Expression),
}

#[allow(unused_braces)]
parser! {
    fn parse_(Token, Span);

    // combine two spans
    (a, b) {
        Span {
            lo: a.lo,
            hi: b.hi,
        }
    }

    program: Program {
        statements[s] => Program { stmts: s }
    }

    statements: Vec<ASTNode> {
        => vec![],
        statements[mut st] assign[e] => {
            st.push(e);
            st
        }
    }

    optionalComa: () {
        => (),
        Comma => (),
    }

    optionalArgs: Vec<String> {
        => Vec::new(),
        optionalArgs[mut a] Ident(ident) optionalComa => {
            a.push(ident);
            a
        }
    }

    /*
        statement        : [declaration | definition];
        declaration      : Extern prototype;
        definition       : Def prototype expression;
        prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
    */

    assign: ASTNode {
        Extern prototype[p] => ASTNode::Extern(p),
        Def prototype[p] term[t] => ASTNode::Function(
            Function{ proto: p , body: Some(t) }
        ),
        Main Do term[t] => ASTNode::Function(
            Function{ proto: Prototype{ name: "main".to_owned(), args: vec![] }, body: Some(t) }
        ),
        term[t] => ASTNode::Expression(t),
    }

    prototype: Prototype {
        Ident(ident) OpeningParenthesis optionalArgs[args] ClosingParenthesis => Prototype{ name: ident, args: args },
    }

    /*
        expression       : [primary_expr (Op primary_expr)*];
        primary_expr     : [Ident | Number | call_expr | parenthesis_expr];
        call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
        parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
    */

    terms: Vec<Expression> {
        => Vec::new(),
        terms[mut ts] term[e] optionalComa => {
            ts.push(e);
            ts
        }
    }

    term: Expression {
        Var Ident(var_name) Eq fact[var_value] In term[body] => Expression::VarIn(var_name, Box::new(var_value), Box::new(body)),
        If fact[cond] Then term[e1] Else term[e2] => Expression::Conditional(Box::new(cond), Box::new(e1), Box::new(e2)),
        For Ident(i) Eq atom[start] Comma fact[cond] Comma fact[step] Do term[body] => Expression::ForLoop(i, Box::new(start), Box::new(cond), Some(Box::new(step)), Box::new(body)),
        While fact[cond] Do term[body] => Expression::WhileLoop(Box::new(cond), Box::new(body)),
        fact[x] => x,
    }

    fact: Expression {
        Ident(v) Eq atom[t] => Expression::Binary(BinaryOp::Eq, Box::new(Expression::Variable(v)), Box::new(t)),
        atom[x] => x,
        binoperation[b] => b,
    }

    binoperation: Expression {
        fact[lhs] Equiv atom[rhs] => Expression::Binary(BinaryOp::Equiv, Box::new(lhs), Box::new(rhs)),
        fact[lhs] Plus atom[rhs] => Expression::Binary(BinaryOp::Plus, Box::new(lhs), Box::new(rhs)),
        fact[lhs] Minus atom[rhs] => Expression::Binary(BinaryOp::Minus, Box::new(lhs), Box::new(rhs)),
        fact[lhs] Mult atom[rhs] => Expression::Binary(BinaryOp::Mult, Box::new(lhs), Box::new(rhs)),
        fact[lhs] GreaterThan atom[rhs] => Expression::Binary(BinaryOp::GreaterThan, Box::new(lhs), Box::new(rhs)),
        fact[lhs] LowerThan atom[rhs] => Expression::Binary(BinaryOp::LowerThan, Box::new(lhs), Box::new(rhs)),
    }

    atom: Expression {
        Ident(i) => Expression::Variable(i),
        Number(n) => Expression::Number(n),
        functionCall[c] => c,
    }

    functionCall: Expression {
        Ident(i) OpeningParenthesis terms[ts] ClosingParenthesis => Expression::Call(i, ts),
    }
}

pub fn parse<I: Iterator<Item = (Token, Span)>>(
    i: I,
) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
    parse_(i)
}

/// Take top-level expression and makes an anonymous function out of it, for REPL.
pub fn create_toplevel_expr(expr: Expression) -> Function {
    Function {
        proto: Prototype {
            name: format!("anonymous@{}", Uuid::new_v4().to_string()),
            args: vec![],
        },
        body: Some(expr),
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_var_in() {
        let code = r#"var a = 1 in var b = 2 in a + b"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Expression(Expression::VarIn(
                    "a".to_owned(),
                    Box::new(Expression::Number(1.0)),
                    Box::new(Expression::VarIn(
                        "b".to_owned(),
                        Box::new(Expression::Number(2.0)),
                        Box::new(Expression::Binary(
                            BinaryOp::Plus,
                            Box::new(Expression::Variable("a".to_owned())),
                            Box::new(Expression::Variable("b".to_owned()))
                        )),
                    )),
                ))]
            }
        )
    }

    #[test]
    fn test_parse_function() {
        let code = r#"def add(x,y) x + y"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Function(Function {
                    proto: Prototype {
                        name: "add".to_owned(),
                        args: vec!["x".to_owned(), "y".to_owned()]
                    },
                    body: Some(Expression::Binary(
                        BinaryOp::Plus,
                        Box::new(Expression::Variable("x".to_owned())),
                        Box::new(Expression::Variable("y".to_owned()))
                    ))
                })]
            }
        )
    }

    #[test]
    fn test_parse_extern_function() {
        let code = r#"extern cos(x)"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Extern(Prototype {
                    name: "cos".to_owned(),
                    args: vec!["x".to_owned()],
                })]
            }
        )
    }

    #[test]
    fn test_parse_main_function() {
        let code = r#"main do 1 + 1"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Function(Function {
                    proto: Prototype {
                        name: "main".to_owned(),
                        args: vec![]
                    },
                    body: Some(Expression::Binary(
                        BinaryOp::Plus,
                        Box::new(Expression::Number(1.0)),
                        Box::new(Expression::Number(1.0)),
                    ))
                })]
            }
        )
    }

    #[test]
    fn test_parse_if_then_else() {
        let code = r#"if 0 then 1 else 2"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Expression(Expression::Conditional(
                    Box::new(Expression::Number(0.0)),
                    Box::new(Expression::Number(1.0)),
                    Box::new(Expression::Number(2.0))
                ),)]
            }
        )
    }

    #[test]
    fn test_parse_while_loop() {
        let code = r#"while a do a"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Expression(Expression::WhileLoop(
                    Box::new(Expression::Variable("a".to_owned())),
                    Box::new(Expression::Variable("a".to_owned())),
                ),)]
            }
        )
    }

    #[test]
    fn test_parse_for_loop() {
        let code = r#"for a = 1, a < 10, 1 do 4"#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![ASTNode::Expression(Expression::ForLoop(
                    "a".to_owned(),
                    Box::new(Expression::Number(1.0)),
                    Box::new(Expression::Binary(
                        BinaryOp::LowerThan,
                        Box::new(Expression::Variable("a".to_owned())),
                        Box::new(Expression::Number(10.0))
                    )),
                    Some(Box::new(Expression::Number(1.0))),
                    Box::new(Expression::Number(4.0)),
                ))],
            }
        )
    }

    #[test]
    fn test_parse_multilines() {
        let code = r#"
            def add(x,y) x + y

            def sub(x,y) x - y
        "#;

        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize();
        assert!(tokens.len() > 0);

        let progam_ast = parse(tokens.into_iter());

        assert!(progam_ast.is_ok());

        let ast = progam_ast.unwrap();

        assert_eq!(
            ast,
            Program {
                stmts: vec![
                    ASTNode::Function(Function {
                        proto: Prototype {
                            name: "add".to_owned(),
                            args: vec!["x".to_owned(), "y".to_owned()]
                        },
                        body: Some(Expression::Binary(
                            BinaryOp::Plus,
                            Box::new(Expression::Variable("x".to_owned())),
                            Box::new(Expression::Variable("y".to_owned()))
                        ))
                    }),
                    ASTNode::Function(Function {
                        proto: Prototype {
                            name: "sub".to_owned(),
                            args: vec!["x".to_owned(), "y".to_owned()]
                        },
                        body: Some(Expression::Binary(
                            BinaryOp::Minus,
                            Box::new(Expression::Variable("x".to_owned())),
                            Box::new(Expression::Variable("y".to_owned()))
                        ))
                    })
                ]
            }
        )
    }
}
