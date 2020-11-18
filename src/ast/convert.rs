use super::Term;

#[derive(Clone, Debug, Default)]
pub struct Context {
    vars: Vec<String>,
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
    pub fn get<T: ?Sized + PartialEq<str>>(&self, name: &T) -> Option<usize> {
        let mut iter = self.vars.iter();
        let mut pos = 0;

        while let Some(var) = iter.next_back() {
            if T::eq(&name, &var) {
                return Some(pos);
            } else {
                pos += 1;
            }
        }

        None
    }

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
    pub fn push<F: FnOnce(&Self) -> T, T>(&self, var: String, f: F) -> T {
        let mut context = self.clone();
        context.vars.push(var);
        f(&context)
    }
}

pub trait Convert {
    fn convert(self, context: &Context) -> Term;
}
