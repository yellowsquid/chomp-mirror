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

    pub fn with_unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.unguard_points.push(self.vars.len());
        let res = f(self);
        self.unguard_points.pop();
        res
    }

    pub fn get_variable_type(&self, var: Variable) -> Result<&Type, GetVariableError> {
        self.vars
            .iter()
            .nth_back(var.index)
            .ok_or(GetVariableError::FreeVariable)
            .and_then(|ty| {
                self.unguard_points
                    .last()
                    .and_then(|point| {
                        if point + var.index >= self.vars.len() {
                            Some(ty)
                        } else {
                            None
                        }
                    })
                    .ok_or(GetVariableError::GuardedVariable)
            })
    }

    pub fn with_variable_type<F: FnOnce(&mut Self) -> R, R>(&mut self, ty: Type, f: F) -> R {
        self.vars.push(ty);
        let res = f(self);
        self.vars.pop();
        res
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GetVariableError {
    FreeVariable,
    GuardedVariable,
}
