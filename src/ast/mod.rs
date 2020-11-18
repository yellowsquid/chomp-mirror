use std::collections::BTreeSet;

pub mod convert;

const ITER_LIMIT: usize = 16;

type Ident = String;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Epsilon,
    Bottom,
    Literal(String),
    Cat(Box<Term>, Box<Term>),
    Alt(Box<Term>, Box<Term>),
    Fix(Box<Term>), // Uses de Bruijn indices
    Variable(usize),
    Call(Ident, Vec<Term>),
}

fn first_to_null(vars: &[(bool, BTreeSet<char>)]) -> Vec<bool> {
    vars.iter().map(|(null, _)| *null).collect::<Vec<_>>()
}

fn flast_to_null(vars: &[(bool, BTreeSet<char>, BTreeSet<char>)]) -> Vec<bool> {
    vars.iter().map(|(null, _, _)| *null).collect::<Vec<_>>()
}

fn flast_to_first(vars: &[(bool, BTreeSet<char>, BTreeSet<char>)]) -> Vec<(bool, BTreeSet<char>)> {
    vars.iter()
        .map(|(null, first, _)| (*null, first.clone()))
        .collect::<Vec<_>>()
}

fn ctx_to_flast(ctx: &[Term]) -> Vec<(bool, BTreeSet<char>, BTreeSet<char>)> {
    match ctx {
        [] => Vec::new(),
        [.., term] => {
            let mut rest = ctx_to_flast(&ctx[..ctx.len() - 1]);
            let null = term.null(&flast_to_null(&rest));
            let first = term.first(&flast_to_first(&rest));
            let flast = term.flast(&rest);
            rest.push((null, first, flast));
            rest
        }
    }
}

fn ctx_to_first(ctx: &[Term]) -> Vec<(bool, BTreeSet<char>)> {
    match ctx {
        [] => Vec::new(),
        [.., term] => {
            let mut rest = ctx_to_first(&ctx[..ctx.len() - 1]);
            let null = term.null(&first_to_null(&rest));
            let first = term.first(&rest);
            rest.push((null, first));
            rest
        }
    }
}

/// NOTE: This assumes the variables are well-formed. Need to fix in general
impl Term {
    pub fn null(&self, vars: &[bool]) -> bool {
        match self {
            Self::Epsilon => true,
            Self::Bottom => false,
            Self::Literal(s) => s.is_empty(),
            Self::Cat(fst, snd) => fst.null(vars) && snd.null(vars),
            Self::Alt(fst, snd) => fst.null(vars) || snd.null(vars),
            Self::Fix(inner) => {
                let mut res = false;
                let mut last = None;
                let mut vars = vars.to_owned();
                let mut i = 0;

                while last.map(|s| s != res).unwrap_or(true) {
                    if i >= ITER_LIMIT {
                        panic!("Too many iterations")
                    } else {
                        i += 1
                    }

                    last = Some(res);
                    vars.push(res);
                    res = inner.null(&vars);
                    vars.pop();
                }

                res
            }
            Self::Variable(index) => vars[vars.len() - index - 1],
            Self::Call(_ident, _args) => unimplemented!(),
        }
    }

    pub fn first(&self, vars: &[(bool, BTreeSet<char>)]) -> BTreeSet<char> {
        match self {
            Self::Epsilon => BTreeSet::new(),
            Self::Bottom => BTreeSet::new(),
            Self::Literal(s) => {
                let mut set = BTreeSet::new();
                if let Some(c) = s.chars().next() {
                    set.insert(c);
                }
                set
            }
            Self::Cat(fst, snd) => {
                let mut set = fst.first(vars);
                if fst.null(&first_to_null(vars)) {
                    set.append(&mut snd.first(vars))
                }
                set
            }
            Self::Alt(fst, snd) => {
                let mut set = fst.first(vars);
                set.append(&mut snd.first(vars));
                set
            }
            Self::Fix(inner) => {
                let mut res = BTreeSet::new();
                let mut last = None;
                let null = self.null(&first_to_null(vars));
                let mut vars = vars.to_owned();
                let mut i = 0;

                while last.map(|s| s != res).unwrap_or(true) {
                    if i >= ITER_LIMIT {
                        panic!("Too many iterations")
                    } else {
                        i += 1
                    }

                    last = Some(res.clone());
                    vars.push((null, res));
                    res = inner.first(&vars);
                    vars.pop();
                }

                res
            }
            Self::Variable(index) => vars[vars.len() - index - 1].1.clone(),
            Self::Call(_, _) => unimplemented!(),
        }
    }

    pub fn flast(&self, vars: &[(bool, BTreeSet<char>, BTreeSet<char>)]) -> BTreeSet<char> {
        match self {
            Self::Epsilon => BTreeSet::new(),
            Self::Bottom => BTreeSet::new(),
            Self::Literal(_) => BTreeSet::new(),
            Self::Cat(fst, snd) => {
                let mut set = snd.flast(vars);
                if snd.null(&flast_to_null(vars)) {
                    set.append(&mut snd.first(&flast_to_first(vars)));
                    set.append(&mut fst.flast(vars));
                }
                set
            }
            Self::Alt(fst, snd) => {
                let mut set = fst.flast(vars);
                set.append(&mut snd.flast(vars));
                set
            }
            Self::Fix(inner) => {
                let mut res = BTreeSet::new();
                let mut last = None;
                let null = self.null(&flast_to_null(vars));
                let first = self.first(&flast_to_first(vars));
                let mut vars = vars.to_owned();
                let mut i = 0;

                while last.map(|s| s != res).unwrap_or(true) {
                    if i >= ITER_LIMIT {
                        panic!("Too many iterations")
                    } else {
                        i += 1
                    }

                    last = Some(res.clone());
                    vars.push((null, first.clone(), res));
                    res = inner.flast(&vars);
                    vars.pop();
                }

                res
            }
            Self::Variable(index) => vars[vars.len() - index - 1].2.clone(),
            Self::Call(_, _) => unimplemented!(),
        }
    }

    pub fn type_check(self, ctx: &[Self]) -> Result<Typed, TypeError> {
        match self {
            Self::Epsilon => Ok(Typed::Epsilon),
            Self::Bottom => Ok(Typed::Bottom),
            Self::Literal(s) => Ok(Typed::Literal(s)),
            Self::Cat(fst, snd) => {
                let vars = ctx_to_flast(ctx);
                if fst.null(&flast_to_null(&vars)) {
                    Err(TypeError::CatFirstNull(*fst, *snd))
                } else if fst.flast(&vars).intersection(&snd.first(&flast_to_first(&vars))).next().is_some() {
                    Err(TypeError::CatFirstFlastOverlap(*fst, *snd))
                } else {
                    Ok(Typed::Cat(Box::new(fst.type_check(ctx)?), Box::new(snd.type_check(ctx)?)))
                }
            }
            Self::Alt(fst, snd) => {
                let vars = ctx_to_first(ctx);
                let null = first_to_null(&vars);
                if fst.null(&null) && snd.null(&null) {
                    Err(TypeError::AltBothNull(*fst, *snd))
                } else if fst.first(&vars).intersection(&snd.first(&vars)).next().is_some() {
                    Err(TypeError::AltFirstOverlap(*fst, *snd))
                } else {
                    Ok(Typed::Alt(Box::new(fst.type_check(ctx)?), Box::new(snd.type_check(ctx)?)))
                }
            }
            Self::Fix(inner) => {
                let mut ctx = ctx.to_owned();
                ctx.push(Self::Fix(inner.clone()));
                Ok(Typed::Fix(Box::new(inner.type_check(&ctx)?)))
            }
            Self::Variable(index) => {
                if index < ctx.len() {
                    Ok(Typed::Variable(index))
                } else {
                    Err(TypeError::FreeVariable(index))
                }
            }
            Self::Call(_, _) => unimplemented!("No functions yet")
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeError {
    CatFirstNull(Term, Term),
    CatFirstFlastOverlap(Term, Term),
    AltBothNull(Term, Term),
    AltFirstOverlap(Term, Term),
    FreeVariable(usize)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Typed {
    Epsilon,
    Bottom,
    Literal(String),
    Cat(Box<Typed>, Box<Typed>),
    Alt(Box<Typed>, Box<Typed>),
    Fix(Box<Typed>),
    Variable(usize),
    Call(Ident, Vec<Typed>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_epsilon() {
        assert!(Term::Epsilon.null(&[]));
    }

    #[test]
    fn not_null_bottom() {
        assert!(!Term::Bottom.null(&[]));
    }

    #[test]
    fn not_null_normal_literal() {
        assert!(!Term::Literal("x".to_owned()).null(&[]))
    }

    #[test]
    fn null_empty_literal() {
        assert!(Term::Literal("".to_owned()).null(&[]))
    }

    #[test]
    fn null_cat_both_null() {
        assert!(Term::Cat(Box::new(Term::Epsilon), Box::new(Term::Epsilon)).null(&[]))
    }

    #[test]
    fn not_null_cat_first_not_null() {
        assert!(!Term::Cat(Box::new(Term::Bottom), Box::new(Term::Epsilon)).null(&[]))
    }

    #[test]
    fn not_null_cat_second_not_null() {
        assert!(!Term::Cat(Box::new(Term::Epsilon), Box::new(Term::Bottom)).null(&[]))
    }

    #[test]
    fn not_null_alt_both_not_null() {
        assert!(!Term::Alt(Box::new(Term::Bottom), Box::new(Term::Bottom)).null(&[]))
    }

    #[test]
    fn null_alt_first_null() {
        assert!(Term::Alt(Box::new(Term::Epsilon), Box::new(Term::Bottom)).null(&[]))
    }

    #[test]
    fn null_alt_second_null() {
        assert!(Term::Alt(Box::new(Term::Bottom), Box::new(Term::Epsilon)).null(&[]))
    }

    #[test]
    fn not_null_fix_simple_inner() {
        assert!(!Term::Fix(Box::new(Term::Bottom)).null(&[]))
    }

    #[test]
    fn null_fix_simple_inner() {
        assert!(Term::Fix(Box::new(Term::Epsilon)).null(&[]))
    }

    #[test]
    fn not_null_fix_var_inner() {
        assert!(!Term::Fix(Box::new(Term::Variable(0))).null(&[]))
    }

    #[test]
    fn null_fix_var_inner() {
        assert!(Term::Fix(Box::new(Term::Alt(
            Box::new(Term::Epsilon),
            Box::new(Term::Variable(0))
        )))
        .null(&[]))
    }

    #[test]
    fn first_epsilon_empty() {
        assert_eq!(Term::Epsilon.first(&[]), BTreeSet::new())
    }

    #[test]
    fn first_bottom_empty() {
        assert_eq!(Term::Bottom.first(&[]), BTreeSet::new())
    }

    #[test]
    fn first_literal_first_char() {
        let set = vec!['x'].into_iter().collect();
        assert_eq!(Term::Literal("x".to_owned()).first(&[]), set);
        let set = vec!['h'].into_iter().collect();
        assert_eq!(Term::Literal("hello".to_owned()).first(&[]), set);
        assert_eq!(Term::Literal("".to_owned()).first(&[]), BTreeSet::new());
    }

    #[test]
    fn first_cat_first_not_null() {
        let set = vec!['x'].into_iter().collect();
        assert_eq!(
            Term::Cat(
                Box::new(Term::Literal("x".to_owned())),
                Box::new(Term::Literal("y".to_owned()))
            )
            .first(&[]),
            set
        );
    }

    #[test]
    fn first_cat_first_null() {
        let set = vec!['x', 'y'].into_iter().collect();
        assert_eq!(
            Term::Cat(
                Box::new(Term::Alt(
                    Box::new(Term::Epsilon),
                    Box::new(Term::Literal("x".to_owned()))
                )),
                Box::new(Term::Literal("y".to_owned()))
            )
            .first(&[]),
            set
        );
    }

    // TODO: test flast

    #[test]
    fn types_epsilon() {
        assert_eq!(Term::Epsilon.type_check(&[]), Ok(Typed::Epsilon))
    }

    #[test]
    fn types_bottom() {
        assert_eq!(Term::Bottom.type_check(&[]), Ok(Typed::Bottom))
    }

    #[test]
    fn types_literal() {
        assert_eq!(Term::Literal("x".to_owned()).type_check(&[]), Ok(Typed::Literal("x".to_owned())));
        assert_eq!(Term::Literal("".to_owned()).type_check(&[]), Ok(Typed::Literal("".to_owned())))
    }
}
