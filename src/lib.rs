//! Consider a language like so:
//!
//! ```text
//! exp ::= x | i | b | binary | if_expr
//!
//! x ::= <identifiers>
//! i ::= <integers>
//! b ::= true | false
//!
//! binary ::= exp + exp
//!          | exp * exp
//!          | exp and exp
//!          | exp or exp
//!
//! if_expr ::= if exp
//!             then exp
//!             else exp
//!
//! ```
//!
//! with primitive types, [`Integer`](Type::Integer) and [`Boolean`](Type::Boolean),
//! complete the type-checking capabilities provided by [`TypeChecker`](TypeChecker).
use std::{borrow::Borrow, collections::HashMap};

/// The different productions of our grammar
#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Integer(i64),
    Boolean(bool),
    BinOp(Box<BinExpr>),
    If(Box<IfExpr>),
}

/// The Binary expression
///
/// ```
/// binary ::= <lhs> <op> <rhs>
/// ```
#[derive(Debug)]
pub struct BinExpr {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: Op,
}

/// The If expression
///
/// ```
/// if_expr ::= if <condition> then <then> else <else_>
/// ```
#[derive(Debug)]
pub struct IfExpr {
    pub condition: Expr,
    pub then: Expr,
    pub else_: Expr,
}

/// The Binary operations
#[derive(Debug, Copy, Clone)]
pub enum Op {
    // arithmetic
    Add,
    Mul,

    // logical
    And,
    Or,
}

/// All valid types in our language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
}

/// `TypeEnv` tracks the types of variable bindings
#[derive(Debug)]
pub struct TypeEnv(HashMap<String, Type>);

impl TypeEnv {
    /// Initialize an empty environment
    pub fn empty() -> Self {
        Self { 0: HashMap::new() }
    }

    /// Add a (binding, type) pair into the environment
    pub fn extend<K: AsRef<str>>(&mut self, key: K, value: Type) {
        self.0.insert(key.as_ref().to_owned(), value);
    }

    /// Retrieve the type of a binding
    pub fn lookup(&self, key: &str) -> Type {
        *self.0.get(key).expect("unbound variable")
    }
}

/// Check and infer types of expressions
#[derive(Debug)]
pub struct TypeChecker {
    environment: TypeEnv,
}

impl TypeChecker {
    /// Initializes a new type-checker instance
    pub fn new(environment: TypeEnv) -> Self {
        Self { environment }
    }

    /// Infers the type of an expression, and finds type-errors on the way
    pub fn type_of<E: Borrow<Expr>>(&self, expr: E) -> Type {
        let expr = expr.borrow();
        match expr {
            Expr::Integer(_) => Type::Integer,
            Expr::Boolean(_) => Type::Boolean,
            Expr::Ident(ident) => self.environment.lookup(ident),
            Expr::BinOp(bin_expr) => self.type_of_binop(bin_expr),
            Expr::If(if_expr) => self.type_of_if(if_expr),
        }
    }

    // TODO: complete this function:
    // Infers the type of a binary-expression, and finds type-errors on the way
    fn type_of_binop(&self, bin_expr: &BinExpr) -> Type {
        todo!()
    }

    // TODO: complete this function:
    // Infers the type of an if-expression, and finds type-errors on the way
    fn type_of_if(&self, if_expr: &IfExpr) -> Type {
        todo!()
    }
}

// Helpers for tests
#[cfg(test)]
impl Expr {
    pub fn ident(i: &str) -> Self {
        Self::Ident(i.to_owned())
    }

    pub fn integer(i: i64) -> Self {
        Self::Integer(i)
    }

    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn binop<E: Into<Expr>>(lhs: E, op: Op, rhs: E) -> Expr {
        let lhs = lhs.into();
        let rhs = rhs.into();
        Self::BinOp(Box::new(BinExpr { lhs, op, rhs }))
    }

    pub fn if_<E: Into<Expr>>(condition: E, then: E, else_: E) -> Expr {
        let condition = condition.into();
        let then = then.into();
        let else_ = else_.into();
        Self::If(Box::new(IfExpr {
            condition,
            then,
            else_,
        }))
    }
}

#[cfg(test)]
impl From<&str> for Expr {
    fn from(s: &str) -> Self {
        Expr::ident(s)
    }
}

#[cfg(test)]
impl From<i64> for Expr {
    fn from(s: i64) -> Self {
        Expr::integer(s)
    }
}

#[cfg(test)]
impl From<bool> for Expr {
    fn from(s: bool) -> Self {
        Expr::boolean(s)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn type_check_primitives() {
        let type_ck = TypeChecker::new(TypeEnv::empty());

        assert_eq!(type_ck.type_of(Expr::integer(4)), Type::Integer);
        assert_eq!(type_ck.type_of(Expr::boolean(true)), Type::Boolean);
    }

    #[test]
    fn type_check_bindings() {
        let mut type_env = TypeEnv::empty();
        type_env.extend("x", Type::Integer);
        type_env.extend("y", Type::Boolean);
        let type_ck = TypeChecker::new(type_env);

        assert_eq!(type_ck.type_of(Expr::ident("x")), Type::Integer);
        assert_eq!(type_ck.type_of(Expr::ident("y")), Type::Boolean);
    }

    // TODO: complete this test
    #[test]
    fn type_check_binop_int() {
        let mut type_env = TypeEnv::empty();
        type_env.extend("x", Type::Integer);
        type_env.extend("y", Type::Integer);

        let _e = Expr::binop("x", Op::Add, "y");
        todo!()
    }

    // TODO: complete this test
    #[test]
    fn type_check_binop_bool() {
        let mut type_env = TypeEnv::empty();
        type_env.extend("x", Type::Boolean);
        type_env.extend("y", Type::Boolean);

        // x or y
        let _e = Expr::binop("x", Op::Or, "y");
        todo!()
    }

    // TODO: complete this test
    #[test]
    fn type_check_if_expr() {
        let mut type_env = TypeEnv::empty();
        type_env.extend("x", Type::Boolean);
        type_env.extend("y", Type::Boolean);
        type_env.extend("z", Type::Boolean);

        // if x then y else z
        let _e = Expr::if_("x", "y", "z");

        todo!()
    }

    // TODO: write more #[should_panic] tests for type-check errors
}
