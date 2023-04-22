use std::collections::HashMap;
use std::convert::AsRef;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

use lazy_static::lazy_static;
use strum_macros::{AsRefStr, EnumCount};

#[derive(Clone, Debug, PartialEq, EnumCount)]
pub enum TokenKind {
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
    Identifier,
    StringLiteral,
    NumberLiteral,

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

type Number = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenData {
    String(String),
    Number(Number),
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    data: TokenData,
}

impl Token {
    pub fn new(kind: TokenKind, data: TokenData) -> Token {
        Token { kind, data }
    }

    pub fn simple(kind: TokenKind) -> Token {
        Token {
            kind,
            data: TokenData::None,
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("and", Token::simple(TokenKind::And));
        m.insert("class", Token::simple(TokenKind::Class));
        m.insert("else", Token::simple(TokenKind::Else));
        m.insert("false", Token::simple(TokenKind::False));
        m.insert("for", Token::simple(TokenKind::For));
        m.insert("fun", Token::simple(TokenKind::Fun));
        m.insert("if", Token::simple(TokenKind::If));
        m.insert("nil", Token::simple(TokenKind::Nil));
        m.insert("or", Token::simple(TokenKind::Or));
        m.insert("print", Token::simple(TokenKind::Print));
        m.insert("return", Token::simple(TokenKind::Return));
        m.insert("super", Token::simple(TokenKind::Super));
        m.insert("this", Token::simple(TokenKind::This));
        m.insert("true", Token::simple(TokenKind::True));
        m.insert("var", Token::simple(TokenKind::Var));
        m.insert("while", Token::simple(TokenKind::While));
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
            '(' => Self::wrap(TokenKind::LeftParen),
            ')' => Self::wrap(TokenKind::RightParen),
            '{' => Self::wrap(TokenKind::LeftBrace),
            '}' => Self::wrap(TokenKind::RightBrace),
            ',' => Self::wrap(TokenKind::Comma),
            '.' => Self::wrap(TokenKind::Dot),
            '-' => Self::wrap(TokenKind::Minus),
            '+' => Self::wrap(TokenKind::Plus),
            ';' => Self::wrap(TokenKind::Semicolon),
            '*' => Self::wrap(TokenKind::Star),

            '!' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Self::wrap(TokenKind::BangEqual)
                } else {
                    Self::wrap(TokenKind::Bang)
                }
            }
            '=' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Self::wrap(TokenKind::EqualEqual)
                } else {
                    Self::wrap(TokenKind::Equal)
                }
            }
            '<' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Self::wrap(TokenKind::LessEqual)
                } else {
                    Self::wrap(TokenKind::Less)
                }
            }
            '>' => {
                if self.src.peek() == Some(&'=') {
                    self.advance();
                    Self::wrap(TokenKind::GreaterEqual)
                } else {
                    Self::wrap(TokenKind::Greater)
                }
            }
            '/' => {
                if self.src.peek() == Some(&'/') {
                    let mut next = self.advance();
                    while next != Some('\n') {
                        next = self.advance();
                    }
                    self.read_token()
                } else {
                    Self::wrap(TokenKind::Slash)
                }
            }
            ' ' | '\r' | '\t' => self.read_token(),
            '\n' => {
                self.line += 1;
                self.col = 0;
                self.read_token()
            }
            '"' => Some(
                self.read_string_literal()
                    .map(|string| Token::new(TokenKind::StringLiteral, TokenData::String(string))),
            ),
            _ => {
                if c.is_ascii_digit() {
                    Some(Ok(Token::new(
                        TokenKind::NumberLiteral,
                        TokenData::Number(self.read_number(c)),
                    )))
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

    fn wrap(kind: TokenKind) -> Option<Result<Token>> {
        Some(Ok(Token::new(kind, TokenData::None)))
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
            None => Token::new(TokenKind::Identifier, TokenData::String(identifier)),
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

    trait UnwrappingTokenStream {
        fn yank_token(&mut self) -> Token;
    }

    impl<'a> UnwrappingTokenStream for TokenStream<'a> {
        fn yank_token(&mut self) -> Token {
            self.read_token().unwrap().unwrap()
        }
    }

    fn end(token_stream: &mut TokenStream) {
        assert_eq!(token_stream.read_token(), None);
    }

    #[test]
    fn test_number_parsing() {
        let program = "1234.1234";
        let mut token_stream = TokenStream::new(program);
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::NumberLiteral, TokenData::Number(1234.1234))
        );
        end(&mut token_stream)
    }

    #[test]
    fn test_string_literal_parsing() {
        let string = "literally string";
        let program = format!("\"{}\"", string);
        let mut token_stream = TokenStream::new(&program);
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::StringLiteral,
                TokenData::String(string.to_owned())
            )
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
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Fun));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::Identifier,
                TokenData::String("my_function".to_owned())
            )
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("arg1".to_owned()))
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Comma));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("arg2".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftBrace)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightBrace)
        );
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
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::If));
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::NumberLiteral, TokenData::Number(2.0))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::EqualEqual)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::NumberLiteral, TokenData::Number(2.0))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftBrace)
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Print));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::StringLiteral,
                TokenData::String("It works!".to_owned())
            )
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightBrace)
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Else));
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftBrace)
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Print));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::StringLiteral,
                TokenData::String("It doesn't work...".to_owned())
            )
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightBrace)
        );
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
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Class));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::Identifier,
                TokenData::String("Brunch".to_owned())
            )
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Less));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(
                TokenKind::Identifier,
                TokenData::String("Breakfast".to_owned())
            )
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftBrace)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("init".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("meat".to_owned()))
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Comma));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("bread".to_owned()))
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Comma));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("drink".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftBrace)
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Super));
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Dot));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("init".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::LeftParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("meat".to_owned()))
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Comma));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("bread".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightParen)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::Semicolon)
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::This));
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Dot));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("drink".to_owned()))
        );
        assert_eq!(token_stream.yank_token(), Token::simple(TokenKind::Equal));
        assert_eq!(
            token_stream.yank_token(),
            Token::new(TokenKind::Identifier, TokenData::String("drink".to_owned()))
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::Semicolon)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightBrace)
        );
        assert_eq!(
            token_stream.yank_token(),
            Token::simple(TokenKind::RightBrace)
        );
        end(&mut token_stream);
    }
}
