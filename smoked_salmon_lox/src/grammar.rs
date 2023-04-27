use std::fmt::Display;

// TODO: Terminal and NonTerminal are just aliases for a trait bounds
// required for parsing. Unfortunately, until https://doc.rust-lang.org/beta/unstable-book/language-features/trait-alias.html
// stabilizes, there is no ergonomic way to do it, so client code has to write
// impl grammar::Terminal for Terminal {} in order to make it work

pub trait SymbolTrait
where
    Self: PartialEq + Eq + PartialOrd + Ord + Clone
{
    const SIZE: usize;

    fn idx(&self) -> usize;
}

pub trait Terminal : SymbolTrait {}

pub trait NonTerminal : SymbolTrait {}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<T: Terminal, NT: NonTerminal> {
    Terminal(T),
    NonTerminal(NT),
    Epsilon,
}

impl<T: Terminal, NT: NonTerminal> Display for Symbol<T, NT>
where
    T: Display,
    NT: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Symbol::Terminal(term) => term.to_string(),
                Symbol::NonTerminal(nonterm) => nonterm.to_string(),
                Symbol::Epsilon => "ε".to_owned(),
            }
        )
    }
}

#[derive(Clone)]
pub struct Production<T: Terminal, NT: NonTerminal> {
    pub lhs: NT,
    pub rhs: Vec<Symbol<T, NT>>,
}

impl<T: Terminal, NT: NonTerminal> Display for Production<T, NT>
where
    T: Display,
    NT: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rhs = self
            .rhs
            .iter()
            .map(|symbol| symbol.to_string())
            .collect::<Vec<String>>()
            .join(" , ");
        write!(f, "{} = {} ;", self.lhs.to_string(), rhs)
    }
}

pub struct Productions<T: Terminal, NT: NonTerminal> {
    productions: Vec<Production<T, NT>>,
}

impl<T: Terminal, NT: NonTerminal> Productions<T, NT> {
    pub fn new(productions: Vec<Production<T, NT>>) -> Self { Self { productions } }

    pub fn inner(&self) -> &Vec<Production<T, NT>> {
        &self.productions
    }

    pub fn find_productions_from(&self, nonterm: &NT) -> impl Iterator<Item = &Production<T, NT>> {
        self.productions
            .iter()
            .filter(|prod| &prod.lhs == nonterm)
    }

    pub fn find_productions_to(&self, nonterm: &NT) -> impl Iterator<Item = (&Production<T, NT>, usize)>{
        self.productions
            .iter()
            .zip(self.productions.iter()
                .filter_map(|prod| {
                    prod.rhs.iter()
                        .position(|symbol| 
                            if let Symbol::NonTerminal(nonterm_b) = symbol {
                                nonterm_b == nonterm
                            } else {
                                false
                            }
                        )
                })
            )
    }
}

impl<T: Terminal, NT: NonTerminal> Display for Productions<T, NT>
where
    T: Display,
    NT: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let productions = self
            .productions
            .iter()
            .map(|production| production.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{}", productions)
    }
}

pub struct Grammar<T: Terminal, NT: NonTerminal> {
    pub start_symbol: NT,
    pub productions: Productions<T, NT>,
}

impl<T: Terminal, NT: NonTerminal> Display for Grammar<T, NT>
where
    T: Display,
    NT: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "S = {}\n\
             P = (\n\
             {}\n\
             )",
            self.start_symbol, self.productions
        )
    }
}

#[macro_export]
macro_rules! productions {
    ( $( $lhs:expr => $( $rhs_symbol:expr ),+);+ ) => {
        {
            let mut productions = Vec::new();
            $(
                let lhs = $lhs;
                let mut symbols: Vec<Symbol<_, _>> = Vec::new();
                $(
                    symbols.push($rhs_symbol.into());
                )+
                productions.push(Production { lhs, rhs: symbols });
            )+
            Productions { productions }
        }
    };
}

mod tests {
    /* Test cases use simple expression grammar */
    use super::*;
    use lazy_static::lazy_static;
    use strum_macros::{Display, EnumCount};
    use strum::EnumCount;

    #[derive(PartialEq, Eq, Display, EnumCount, Clone, PartialOrd, Ord)]
    enum ExprNonTerminal {
        E,
        T,
        Eprim,
        Tprim,
        F,
    }

    #[derive(PartialEq, Eq, Display, EnumCount, Clone, PartialOrd, Ord)]
    enum ExprTerminal {
        Plus,
        LeftParen,
        RightParen,
        Star,
        Identifier,
    }

    impl SymbolTrait for ExprTerminal {
        const SIZE: usize = Self::COUNT;

        fn idx(&self) -> usize {
            *self as usize
        }
    }  
    impl Terminal for ExprTerminal {}

    impl SymbolTrait for ExprNonTerminal {
        const SIZE: usize = Self::COUNT;

        fn idx(&self) -> usize {
            *self as usize
        }
    }
    impl NonTerminal for ExprNonTerminal {}

    impl From<ExprNonTerminal> for Symbol<ExprTerminal, ExprNonTerminal> {
        fn from(value: ExprNonTerminal) -> Self {
            Symbol::NonTerminal(value)
        }
    }

    impl From<ExprTerminal> for Symbol<ExprTerminal, ExprNonTerminal> {
        fn from(value: ExprTerminal) -> Self {
            Symbol::Terminal(value)
        }
    }

    lazy_static! {
        static ref GRAMMAR: Grammar<ExprTerminal, ExprNonTerminal> = {
            use ExprNonTerminal::*;
            use ExprTerminal::*;

            let productions = productions!(
                E => T , Eprim ;
                Eprim => Plus , T , Eprim ;
                Eprim => Symbol::Epsilon ;
                T => F , Tprim ;
                Tprim => Star , F , Tprim ;
                Tprim => Symbol::Epsilon ;
                F => LeftParen , E , RightParen ;
                F => Identifier
            );

            Grammar {
                start_symbol: ExprNonTerminal::E,
                productions,
            }
        };
    }
}
