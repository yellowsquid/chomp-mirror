use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use super::Term;

#[derive(Debug, Default)]
/// The variable context for converting a concrete syntax tree to an abstract
/// one.
///
/// There are two name scopes in a context. Bindings are used by fixed points.
/// Variables are formal parameters of macros.
pub struct Context {
    bindings: Vec<String>,
    variables: HashMap<String, usize>,
}

impl Context {
    /// Creates a new variable context. No names are bound.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns [`Some`] index of a binding `name`.
    ///
    /// # Errors
    /// Returns [`None`] if `name.is_empty()` or if `name` is unbound.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let mut context = Context::new();
    /// assert_eq!(context.get_binding("x"), None);
    ///
    /// context.with_binding("x".to_owned(), |c| {
    ///     assert_eq!(c.get_binding("x"), Some(0));
    ///
    ///     c.with_binding("y".to_owned(), |c| {
    ///         assert_eq!(c.get_binding("x"), Some(1));
    ///     })
    /// });
    ///
    /// assert_eq!(context.get_binding("x"), None);
    /// ```
    pub fn get_binding<T: ?Sized + PartialEq<str>>(&self, name: &T) -> Option<usize> {
        let mut iter = self.bindings.iter();
        let mut pos = 0;

        if name == "" {
            return None;
        }

        while let Some(var) = iter.next_back() {
            if T::eq(&name, &var) {
                return Some(pos);
            } else {
                pos += 1;
            }
        }

        None
    }

    /// Adds a binding `name` for the duration of the function `f`.
    ///
    /// # Panics
    /// If `name.is_empty()`.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let mut context = Context::new();
    /// assert_eq!(context.get_binding("x"), None);
    ///
    /// context.with_binding("x".to_owned(), |c| {
    ///     assert_eq!(c.get_binding("x"), Some(0));
    /// });
    ///
    /// assert_eq!(context.get_binding("x"), None);
    /// ```
    pub fn with_binding<F: FnOnce(&mut Self) -> T, T>(&mut self, name: String, f: F) -> T {
        if name.is_empty() {
            panic!()
        }

        self.bindings.push(name);
        let res = f(self);
        self.bindings.pop();
        res
    }

    /// Returns [`Some`] index of a variable `name`.
    ///
    /// # Errors
    /// If `name == "".to_owned().borrow()` or `name` is unbound.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let mut ctx = Context::new();
    /// assert_eq!(ctx.get_variable("x"), None);
    ///
    /// ctx.set_variables(vec!["x".to_owned(), "y".to_owned()]);
    /// assert_eq!(ctx.get_variable("x"), Some(0));
    /// assert_eq!(ctx.get_variable("y"), Some(1));
    ///
    /// ctx.set_variables(vec![]);
    /// assert_eq!(ctx.get_variable("x"), None);
    /// ```
    pub fn get_variable<T: ?Sized + Hash + Eq>(&self, name: &T) -> Option<usize>
    where
        String: Borrow<T>,
    {
        if name == "".to_owned().borrow() {
            return None;
        }

        self.variables.get(name).copied()
    }

    /// Reassigns all variable indices based off of their position in `args`.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let mut ctx = Context::new();
    /// assert_eq!(ctx.get_variable("x"), None);
    ///
    /// ctx.set_variables(vec!["x".to_owned(), "y".to_owned()]);
    /// assert_eq!(ctx.get_variable("x"), Some(0));
    /// assert_eq!(ctx.get_variable("y"), Some(1));
    ///
    /// ctx.set_variables(vec![]);
    /// assert_eq!(ctx.get_variable("x"), None);
    /// ```
    pub fn set_variables<I: IntoIterator<Item = String>>(&mut self, args: I) {
        self.variables = args
            .into_iter()
            .enumerate()
            .map(|(index, name)| (name, index))
            .collect();
    }
}

/// Used to convert a concrete term into an abstract term.
pub trait Convert {
    /// Performs the conversion.
    fn convert(&self, context: &mut Context) -> Term;
}
