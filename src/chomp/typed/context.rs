use crate::chomp::ast::Variable;

use super::Type;

#[derive(Debug, Default)]
pub struct Context {
    vars: Vec<Type>,
    unguard_points: Vec<usize>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn depth(&self) -> usize {
        self.vars.len()
    }

    pub fn is_unguarded(&self, var: &Variable) -> Option<bool> {
        if self.vars.len() <= var.index {
            None
        } else if self.unguard_points.is_empty() {
            Some(false)
        } else {
            Some(self.unguard_points[self.unguard_points.len() - 1] + var.index >= self.vars.len())
        }
    }

    pub fn with_unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.unguard_points.push(self.vars.len());
        let res = f(self);
        self.unguard_points.pop();
        res
    }

    pub fn get_variable_type(&self, var: &Variable) -> Result<&Type, GetVariableError> {
        match self.is_unguarded(var) {
            None => Err(GetVariableError::FreeVariable),
            Some(false) => Err(GetVariableError::GuardedVariable),
            Some(true) => Ok(&self.vars[self.vars.len() - var.index - 1]),
        }
    }

    pub fn with_variable_type<F: FnOnce(&mut Self) -> R, R>(&mut self, ty: Type, f: F) -> R {
        self.vars.push(ty);
        let res = f(self);
        self.vars.pop();
        res
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GetVariableError {
    FreeVariable,
    GuardedVariable,
}
