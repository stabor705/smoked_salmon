use crate::token_stream::TokenKind;

use std::fmt::Display;

use lazy_static::lazy_static;
use strum::EnumCount;
use strum_macros::{AsRefStr, EnumCount};

type Terminal = TokenKind;

/// Set of nonterminal symbols used by formal grammar.
#[derive(EnumCount, AsRefStr)]
enum NonTerminal {
    Expr,
    Equality,
    EqualityRep,
    EqualityOps,
    Comparison,
    ComparisonRep,
    ComparisonOps,
    Term,
    TermRep,
    TermOps,
    Factor,
    FactorRep,
    FactorOps,
    Unary,
    UnaryOps,
    Primary,
}

enum Symbol {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
    Epsilon,
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        match self {
            Symbol::Terminal(term) => term.as_ref(),
            Symbol::NonTerminal(nonterm) => nonterm.as_ref(),
            Symbol::Epsilon => "ε",
        }
    }
}

impl From<Terminal> for Symbol {
    fn from(value: Terminal) -> Self {
        Self::Terminal(value)
    }
}

impl From<NonTerminal> for Symbol {
    fn from(value: NonTerminal) -> Self {
        Self::NonTerminal(value)
    }
}

struct Production {
    lhs: NonTerminal,
    rhs: Vec<Symbol>,
}

impl Production {
    pub fn new(lhs: NonTerminal, rhs: Vec<Symbol>) -> Production {
        Production { lhs, rhs }
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rhs = self
            .rhs
            .iter()
            .map(|symbol| symbol.as_ref())
            .collect::<Vec<&str>>()
            .join(" , ");
        write!(f, "{} = {} ;", self.lhs.as_ref(), rhs)
    }
}

pub struct Grammar {
    start_symbol: NonTerminal,
    productions: Vec<Production>,
}

/// Display implementation for Grammar that produces EBNF notation.
impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let productions = self
            .productions
            .iter()
            .map(|production| production.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        write!(
            f,
            "S = {}\n\
             P = (\n\
             {}\n\
             )",
            self.start_symbol.as_ref(),
            productions
        )
    }
}

macro_rules! productions {
    ( $( $lhs:expr => $( $rhs_symbol:expr ),+);+ ) => {
        {
            let mut productions = Vec::new();
            $(
                let lhs: NonTerminal = $lhs;
                let mut symbols: Vec<Symbol> = Vec::new();
                $(
                    symbols.push($rhs_symbol.into());
                )+
                productions.push(Production { lhs, rhs: symbols });
            )+
            productions
        }
    };
}

lazy_static! {
    pub static ref GRAMMAR: Grammar = {
        // Formal grammar of Lox language is defined in **Parsing Expressions**
        // chapter https://craftinginterpreters.com/parsing-expressions.html#ambiguity-and-the-parsing-game
        use NonTerminal::*;
        use TokenKind::*;

        let productions = productions!(
            Expr => Equality ;

            Equality => Comparison , EqualityRep ;
            EqualityRep => Symbol::Epsilon ;
            EqualityRep => EqualityOps , Comparison , EqualityRep ;
            EqualityOps => BangEqual ;
            EqualityOps => EqualEqual ;

            Comparison => Term , ComparisonRep ;
            ComparisonRep => Symbol::Epsilon ;
            ComparisonRep => ComparisonOps , Term , ComparisonRep ;
            ComparisonOps => Greater ;
            ComparisonOps => GreaterEqual ;
            ComparisonOps => Less ;
            ComparisonOps => LessEqual ;

            Term => Factor , TermRep ;
            TermRep => Symbol::Epsilon ;
            TermRep => TermOps , Factor , TermRep ;
            TermOps => Minus ;
            TermOps => Plus ;

            Factor => Unary , FactorRep ;
            FactorRep => Symbol::Epsilon ;
            FactorRep => FactorOps , Unary , FactorRep ;
            FactorOps => Slash ;
            FactorOps => Star ;

            Unary => UnaryOps , Unary ;
            Unary => Primary ;
            UnaryOps => Bang ;
            UnaryOps => Minus ;

            Primary => NumberLiteral ;
            Primary => StringLiteral ;
            Primary => True ;
            Primary => False ;
            Primary => Nil ;
            Primary => LeftParen , Expr , RightParen
        );
        Grammar {
            start_symbol: NonTerminal::Expr,
            productions,
        }
    };
}

type ParsingTable = [[Production; TokenKind::COUNT]; NonTerminal::COUNT];

fn construct_parsing_table(grammar: Grammar) -> ParsingTable {
    todo!();
}

struct Parser {
    stack: Vec<Symbol>,
    parsing_table: ParsingTable,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Parser {
        Parser {
            stack: Vec::new(),
            parsing_table: construct_parsing_table(grammar),
        }
    }
}
