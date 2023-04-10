use std::collections::HashMap;
use std::convert::AsRef;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

use lazy_static::lazy_static;
use strum_macros::AsRefStr;

type Number = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(Number),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("and", Token::And);
        m.insert("class", Token::Class);
        m.insert("else", Token::Else);
        m.insert("false", Token::False);
        m.insert("for", Token::For);
        m.insert("fun", Token::Fun);
        m.insert("if", Token::If);
        m.insert("nil", Token::Nil);
        m.insert("or", Token::Or);
        m.insert("print", Token::Print);
        m.insert("return", Token::Return);
        m.insert("super", Token::Super);
        m.insert("this", Token::This);
        m.insert("true", Token::True);
        m.insert("var", Token::Var);
        m.insert("while", Token::While);
        m
    };
}

#[derive(AsRefStr, Debug, PartialEq)]
pub enum LexicalErrorKind {
    UnrecognizedToken,
    UnterminatedStringLiteral,
}

#[derive(Debug, PartialEq)]
pub struct LexicalError {
    kind: LexicalErrorKind,
    line: u32,
    col: u32,
}

impl LexicalError {
    pub fn new(kind: LexicalErrorKind, line: u32, col: u32) -> LexicalError {
        LexicalError { kind, line, col }
    }
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexical analysis error: {}. Line: {}, col: {}",
            self.kind.as_ref(),
            self.line,
            self.col
        )
    }
}

type Result<T> = std::result::Result<T, LexicalError>;

pub struct TokenStream<'a> {
    src: Peekable<Chars<'a>>,
    line: u32,
    col: u32,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str) -> TokenStream {
        return TokenStream {
            src: src.chars().peekable(),
            line: 0,
            col: 0,
        };
    }

    fn read_token(&mut self) -> Option<Result<Token>> {
        let c = self.advance()?;

        match c {
            '(' => Some(Ok(Token::LeftParen)),
            ')' => Some(Ok(Token::RightParen)),
            '{' => Some(Ok(Token::LeftBrace)),
            '}' => Some(Ok(Token::RightBrace)),
            ',' => Some(Ok(Token::Comma)),
            '.' => Some(Ok(Token::Dot)),
            '-' => Some(Ok(Token::Minus)),
            '+' => Some(Ok(Token::Plus)),
            ';' => Some(Ok(Token::Semicolon)),
            '*' => Some(Ok(Token::Star)),

            '!' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Some(Ok(Token::BangEqual))
                } else {
                    Some(Ok(Token::Bang))
                }
            }
            '=' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Some(Ok(Token::EqualEqual))
                } else {
                    Some(Ok(Token::Equal))
                }
            }
            '<' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Some(Ok(Token::LessEqual))
                } else {
                    Some(Ok(Token::Less))
                }
            }
            '>' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Some(Ok(Token::GreaterEqual))
                } else {
                    Some(Ok(Token::Greater))
                }
            }
            '/' => {
                if self.src.peek() == Some(&'/') {
                    let mut next = self.advance();
                    while next != Some('\n') {
                        next = self.advance();
                    }
                    self.next()
                } else {
                    Some(Ok(Token::Slash))
                }
            }
            ' ' | '\r' | '\t' => self.next(),
            '\n' => {
                self.line += 1;
                self.col = 0;
                self.next()
            }
            '"' => Some(
                self.read_string_literal()
                    .map(|string| Token::StringLiteral(string)),
            ),
            _ => {
                if c.is_ascii_digit() {
                    Some(Ok(Token::NumberLiteral(self.read_number(c))))
                } else if c.is_alphabetic() {
                    Some(Ok(self.read_identifier(c)))
                } else {
                    Some(Err(LexicalError::new(
                        LexicalErrorKind::UnrecognizedToken,
                        self.line,
                        self.col,
                    )))
                }
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.col += 1;
        self.src.next()
    }

    fn read_string_literal(&mut self) -> Result<String> {
        let mut string = String::new();
        loop {
            let c = self.advance().ok_or(LexicalError::new(
                LexicalErrorKind::UnterminatedStringLiteral,
                self.line,
                self.col,
            ))?;
            if c == '"' {
                break;
            }
            string.push(c);
        }
        Ok(string)
    }

    fn read_number(&mut self, first_c: char) -> Number {
        let mut num = String::from(first_c);
        let mut dot_appeared = false;
        loop {
            let c = self.src.peek();
            match c {
                Some(c) => {
                    if c == &'.' && !dot_appeared {
                        dot_appeared = true;
                        num.push(self.advance().unwrap());
                    } else if c.is_ascii_digit() {
                        num.push(self.advance().unwrap());
                    } else {
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }
        num.parse::<Number>().unwrap()
    }

    fn read_identifier(&mut self, first_c: char) -> Token {
        let mut identifier = String::from(first_c);
        loop {
            let c = self.src.peek();
            match c {
                Some(c) => {
                    if c.is_alphanumeric() || c == &'_' {
                        identifier.push(self.advance().unwrap());
                    } else {
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }
        match KEYWORDS.get(identifier.as_str()) {
            Some(token) => token.clone(),
            None => Token::Identifier(identifier),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn end(token_stream: &mut TokenStream) {
        assert_eq!(token_stream.read_token(), None);
    }

    #[test]
    fn test_number_parsing() {
        let program = "1234.1234";
        let mut token_stream = TokenStream::new(program);
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::NumberLiteral(1234.1234)))
        );
        end(&mut token_stream)
    }

    #[test]
    fn test_string_literal_parsing() {
        let string = "literally string";
        let program = format!("\"{}\"", string);
        let mut token_stream = TokenStream::new(&program);
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::StringLiteral(string.to_owned())))
        );
        end(&mut token_stream)
    }

    #[test]
    fn test_unterminated_string_literal() {
        let mut token_stream = TokenStream::new("\"this is not the end");
        let kind = token_stream.read_token().unwrap().unwrap_err().kind;
        assert_eq!(kind, LexicalErrorKind::UnterminatedStringLiteral);
        end(&mut token_stream);
    }

    #[test]
    fn test_function_def() {
        let mut token_stream = TokenStream::new("fun my_function(arg1, arg2) {}");
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Fun)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("my_function".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftParen)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("arg1".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Comma)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("arg2".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightParen)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightBrace)));
        end(&mut token_stream);
    }

    #[test]
    fn test_condition() {
        let program = "if (2 == 2) {\n
                        print \"It works!\"\n
                      } else {\n
                        print \"It doesn't work...\"\n
                      }";
        let mut token_stream = TokenStream::new(program);
        assert_eq!(token_stream.read_token(), Some(Ok(Token::If)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftParen)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::NumberLiteral(2.0)))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::EqualEqual)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::NumberLiteral(2.0)))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightParen)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Print)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::StringLiteral("It works!".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Else)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Print)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::StringLiteral("It doesn't work...".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightBrace)));
    }

    #[test]
    fn test_class() {
        let program = "class Brunch < Breakfast {\n
                         init(meat, bread, drink) {\n
                           super.init(meat, bread);\n
                           this.drink = drink;\n
                         }\n
                       }";
        let mut token_stream = TokenStream::new(program);
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Class)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("Brunch".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Less)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("Breakfast".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftBrace)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("init".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftParen)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("meat".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Comma)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("bread".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Comma)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("drink".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightParen)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Super)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Dot)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("init".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::LeftParen)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("meat".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Comma)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("bread".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightParen)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Semicolon)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::This)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Dot)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("drink".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Equal)));
        assert_eq!(
            token_stream.read_token(),
            Some(Ok(Token::Identifier("drink".to_owned())))
        );
        assert_eq!(token_stream.read_token(), Some(Ok(Token::Semicolon)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightBrace)));
        assert_eq!(token_stream.read_token(), Some(Ok(Token::RightBrace)));
        end(&mut token_stream);
    }
}
