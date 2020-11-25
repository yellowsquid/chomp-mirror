use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;
use std::rc::Rc;

use super::Term;

#[derive(Debug, Default)]
pub struct Context {
    bindings: Vec<String>,
    variables: HashMap<String, Term>,
    functions: HashMap<String, (Rc<dyn Convert>, Vec<String>)>,
}

impl Context {
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let context = Context::new();
    /// assert_eq!(context.get("x"), None);
    /// assert_eq!(context.get("y"), None);
    /// assert_eq!(context.get("z"), None);
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// # Errors
    /// Returns [`None`] if `name.is_empty()` or if `name` is unbound.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let context = Context::new();
    /// assert_eq!(context.get("x"), None);
    ///
    /// context.push("x".to_owned(), |c| {
    ///     assert_eq!(c.get("x"), Some(0));
    ///
    ///     c.push("y".to_owned(), |c| {
    ///         assert_eq!(c.get("x"), Some(1));
    ///     })
    /// });
    ///
    /// assert_eq!(context.get("x"), None);
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

    /// # Panics
    /// If `name.is_empty()`.
    ///
    /// # Examples
    /// ```
    /// use chomp::ast::convert::Context;
    ///
    /// let context = Context::new();
    /// assert_eq!(context.get("x"), None);
    ///
    /// context.push("x".to_owned(), |c| {
    ///     assert_eq!(c.get("x"), Some(0));
    /// });
    ///
    /// assert_eq!(context.get("x"), None);
    /// ```
    pub fn push_binding<F: FnOnce(&mut Self) -> T, T>(&mut self, name: String, f: F) -> T {
        if name.is_empty() {
            panic!()
        }

        self.bindings.push(name);
        let res = f(self);
        self.bindings.pop();
        res
    }

    /// # Errors
    /// If `name == "".to_owned().borrow()` or `name` is unbound.
    pub fn get_variable<T: ?Sized + Hash + Eq>(&self, name: &T) -> Option<&Term>
    where
        String: Borrow<T>,
    {
        if name == "".to_owned().borrow() {
            return None
        }

        self.variables.get(name)
    }

    /// # Panics
    /// If any variable name is empty.
    pub fn add_function(&mut self, name: String, source: Rc<dyn Convert>, variables: Vec<String>) {
        if variables.iter().any(|s| s.is_empty()) {
            panic!()
        }

        self.functions.insert(name, (source, variables));
    }

    /// This uses dynamic scope for bindings.
    /// # Errors
    /// If `name` is unbound or has been called with the wrong number of arguments.
    pub fn call_function<I: IntoIterator<Item = Term>, T: ?Sized + Hash + Eq>(
        &mut self,
        name: &T,
        args: I,
    ) -> Option<Term>
    where
        String: Borrow<T>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        let (term, vars) = self.functions.get(name)?;
        let args_iter = args.into_iter();

        if vars.len() != args_iter.len() {
            None
        } else {
            let mut old = Vec::new();
            for (var, value) in vars.clone().into_iter().zip(args_iter) {
                if let Some((old_name, old_value)) = self.variables.remove_entry(var.borrow()) {
                    let mut indices = Vec::new();

                    for (index, binding) in self.bindings.iter_mut().enumerate() {
                        if *binding == old_name {
                            indices.push((index, mem::take(binding)));
                        }
                    }

                    old.push((old_name, old_value, indices))
                }

                self.variables.insert(var, value);
            }

            let res = Some(term.clone().convert(self));

            for (name, value, indices) in old {
                for (index, binding) in indices {
                    self.bindings[index] = binding
                }

                self.variables.insert(name, value);
            }

            res
        }
    }
}

pub trait Convert: std::fmt::Debug {
    fn convert(&self, context: &mut Context) -> Term;
}
