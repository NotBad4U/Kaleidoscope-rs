use plex::lexer;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,
    Comment,
    Whitespace,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Plus,
    Minus,
    Mult,
    Ident(String),
    Number(f64),
    If,
    Then,
    Else,
    LowerThan,
    GreaterThan,
    For,
    In,
    Eq,
    OpeningBracket,
    ClosingBracket,
    While,
    Var,
    Equiv,
    Do,
    Main,
}

lexer! {
    fn next_token(text) -> Token;

    r#"[ \t\r\n]+"# => Token::Whitespace,

    r#"[#].*"# => Token::Comment,

    r#"[0-9]+"# => {
        if let Ok(i) = text.parse() {
            Token::Number(i)
        } else {
            panic!("integer {} is out of range", text)
        }
    }

    r#"def"# => Token::Def,
    r#"extern"# => Token::Extern,
    r#"if"# => Token::If,
    r#"then"# => Token::Then,
    r#"else"# => Token::Else,
    r#"for"# => Token::For,
    r#"in"# => Token::In,
    r#"while"# => Token::While,
    r#"var"# => Token::Var,
    r#"do"# => Token::Do,
    r#"main"# => Token::Main,

    r#"\("# => Token::OpeningParenthesis,
    r#"\)"# => Token::ClosingParenthesis,
    r#","# => Token::Comma,
    r#"\+"# => Token::Plus,
    r#"\-"# => Token::Minus,
    r#"\*"# => Token::Mult,
    r#"\<"# => Token::LowerThan,
    r#"\>"# => Token::GreaterThan,
    r#"\="# => Token::Eq,
    r#"\=\="# => Token::Equiv,
    r#"{"# => Token::OpeningBracket,
    r#"}"# => Token::ClosingBracket,

    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => Token::Ident(text.to_owned()),


    r#"."# => panic!("unexpected character: {}", text),
}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer {
            original: s,
            remaining: s,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();

                self.remaining = new_remaining;

                (tok, Span { lo, hi })
            } else {
                return None;
            };

            match tok {
                Token::Whitespace | Token::Comment => {
                    continue;
                }
                tok => {
                    return Some((tok, span));
                }
            }
        }
    }
}

impl<'a> Lexer<'a> {
    /// tokenize ignore Whitespace and Comment
    pub fn tokenize(&mut self) -> Vec<(Token, Span)> {
        let mut result = Vec::new();

        while !self.remaining.is_empty() {
            if let Some((token, span)) = self.next() {
                result.push((token, span))
            }
        }

        result
    }
}
