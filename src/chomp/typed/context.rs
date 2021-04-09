use std::collections::HashMap;

use crate::chomp::ast::Parameter;

use super::{GroundType, Type};

#[derive(Default)]
pub struct Context {
    vars: Vec<(Type, bool)>,
    globals: HashMap<String, Type>,
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

    pub fn get_param_type(&self, param: Parameter) -> Result<&Type, GetParameterError> {
        self.vars
            .iter()
            .nth_back(param.index)
            .ok_or(GetParameterError::FreeParameter)
            .and_then(|(ty, unguard)| {
                if *unguard {
                    Ok(ty)
                } else {
                    self.unguard_points
                        .last()
                        .and_then(|point| {
                            if point + param.index >= self.vars.len() {
                                Some(ty)
                            } else {
                                None
                            }
                        })
                        .ok_or(GetParameterError::GuardedParameter)
                }
            })
    }

    pub fn with_fixed_point_type<F: FnOnce(&mut Self) -> R, R>(&mut self, ty: Type, f: F) -> R {
        self.vars.push((ty, false));
        let res = f(self);
        self.vars.pop();
        res
    }

    pub fn with_parameter_type<F: FnOnce(&mut Self) -> R, R>(&mut self, ty: Type, f: F) -> R {
        self.vars.push((ty, true));
        let res = f(self);
        self.vars.pop();
        res
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GetParameterError {
    FreeParameter,
    GuardedParameter,
}
