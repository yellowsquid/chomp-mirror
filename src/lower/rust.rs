use std::collections::{BTreeSet, HashMap};

use proc_macro2::{TokenStream, TokenTree};
use quote::{format_ident, quote};

use crate::chomp::typed::{Alt, Cat, Epsilon, Fix, Literal, Typed, TypedExpression, Variable};

use super::{Backend, GenerateCode};

#[derive(Debug)]
pub struct RustBackend {
    // Indexed by ID, stores type, impl and dependencies
    data: Vec<(TokenStream, TokenStream, BTreeSet<usize>)>,
    eps_id: Option<usize>,
    lit_map: HashMap<String, usize>,
    cat_map: HashMap<(usize, usize), usize>,
    alt_map: HashMap<(usize, usize), usize>,
    fix_map: HashMap<Box<TypedExpression>, usize>,
    var_map: HashMap<usize, usize>, // Key is fix point ID
    context: Vec<usize>,
}

impl Default for RustBackend {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            eps_id: None,
            lit_map: HashMap::new(),
            cat_map: HashMap::new(),
            alt_map: HashMap::new(),
            fix_map: HashMap::new(),
            var_map: HashMap::new(),
            context: Vec::new(),
        }
    }
}

impl Backend for RustBackend {
    type Id = usize;

    type Code = TokenStream;

    fn gen_epsilon(&mut self, _eps: Epsilon) -> Self::Id {
        match self.eps_id {
            Some(id) => id,
            None => {
                let id = self.data.len();
                let ty = quote! { () };
                let tokens = quote! {
                    impl Parse for () {
                        fn parse<P: Parser>(_input: &mut P) -> Result<Self> {
                            Ok(())
                        }
                    }
                };
                self.data.push((ty, tokens, BTreeSet::new()));
                id
            }
        }
    }

    fn gen_literal(&mut self, lit: Literal) -> Self::Id {
        let lit = lit.inner();
        if let Some(&id) = self.lit_map.get(&lit.value()) {
            id
        } else {
            let id = self.data.len();
            let name = format_ident!("Lit{}", id);
            let doc_name = format!(
                r#"The literal string `"{}"`."#,
                lit.value().escape_debug().collect::<String>()
            );
            let tokens = quote! {
                #[doc=#doc_name]
                pub struct #name;

                impl Parse for #name {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        input.take_str(#lit).map(|()| #name)
                    }
                }
            };

            self.data
                .push((TokenTree::from(name).into(), tokens, BTreeSet::new()));
            self.lit_map.insert(lit.value(), id);

            id
        }
    }

    fn gen_cat(&mut self, cat: Cat) -> Self::Id {
        let (fst, _punct, snd) = cat.unwrap();
        let fst = fst.gen(self);
        let snd = snd.gen(self);

        if let Some(&id) = self.cat_map.get(&(fst, snd)) {
            id
        } else {
            let id = self.data.len();
            let fst_ty = self.data[fst].0.clone();
            let snd_ty = self.data[snd].0.clone();
            self.data.push((
                quote! {(#fst_ty, #snd_ty)},
                TokenStream::new(),
                [fst, snd].iter().cloned().collect(),
            ));
            self.cat_map.insert((fst, snd), id);
            id
        }
    }

    fn gen_alt(&mut self, alt: Alt) -> Self::Id {
        let iter_first = alt.get_type().first_set().clone().into_iter();

        let (left, _punct, right) = alt.unwrap();
        let left_ty = left.get_type();
        let right_ty = right.get_type();

        let left_null = left_ty.nullable();
        let left_first = left_ty.first_set().clone();

        let right_null = right_ty.nullable();
        let right_first = right_ty.first_set().clone();

        let left = left.gen(self);
        let right = right.gen(self);

        if let Some(&id) = self.alt_map.get(&(left, right)) {
            id
        } else {
            let id = self.data.len();
            let left_ty = self.data[left].0.clone();
            let right_ty = self.data[right].0.clone();
            let name = format_ident!("Alt{}", id);
            let mut tokens = quote! {
                pub enum #name {
                    Left(#left_ty),
                    Right(#right_ty),
                }
            };

            let other = if left_null {
                let iter = right_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek() {
                                #(Some(#iter))|* => input.parse().map(Self::Right),
                                _ => input.parse().map(Self::Left),
                            }
                        }
                    }
                }
            } else if right_null {
                let iter = left_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek() {
                                #(Some(#iter))|* => input.parse().map(Self::Left),
                                _ => input.parse().map(Self::Right),
                            }
                        }
                    }
                }
            } else {
                let iter_left = left_first.into_iter();
                let iter_right = right_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek().ok_or(Error::EndOfStream)? {
                                #(#iter_left)|* => input.parse().map(Self::Left),
                                #(#iter_right)|* => input.parse().map(Self::Right),
                                c => input.error(Error::BadBranch(c, &[#(#iter_first),*]))
                            }
                        }
                    }
                }
            };

            tokens.extend(other);
            self.data.push((
                TokenTree::from(name).into(),
                tokens,
                [left, right].iter().cloned().collect(),
            ));
            self.alt_map.insert((left, right), id);
            id
        }
    }

    fn gen_fix(&mut self, fix: Fix) -> Self::Id {
        let (_arg, inner, _span) = fix.unwrap();
        if let Some(&id) = self.fix_map.get(&inner) {
            id
        } else {
            let id = self.data.len();
            let name = format_ident!("Fix{}", id);
            self.data.push((
                TokenTree::from(name.clone()).into(),
                TokenStream::new(),
                BTreeSet::new(),
            ));
            self.context.push(id);
            let inner = inner.gen(self);
            let inner_ty = self.data[inner].0.clone();
            let tokens = quote! {
                pub struct #name(#inner_ty);

                impl Parse for #name {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        input.parse().map(Self)
                    }
                }
            };
            self.data[id].1 = tokens;
            self.data[id].2 = [inner].iter().cloned().collect();
            id
        }
    }

    fn gen_variable(&mut self, var: Variable) -> Self::Id {
        let fix_id = self.context[self.context.len() - var.index() - 1];
        if let Some(&id) = self.var_map.get(&fix_id) {
            id
        } else {
            let id = self.data.len();
            let fix_ty = self.data[fix_id].0.clone();
            let name = quote! {Box<#fix_ty>};
            self.data.push((name, TokenStream::new(), BTreeSet::new()));
            self.var_map.insert(fix_id, id);
            id
        }
    }

    fn emit_code(self, id: Self::Id) -> Self::Code {
        let root = self.data[id].clone();
        let mut tokens = root.1;
        let mut completed = [id].iter().cloned().collect::<BTreeSet<_>>();
        let mut todo = root.2;

        while !todo.is_empty() {
            let mut next = BTreeSet::new();
            completed.extend(todo.clone());

            for ty in todo {
                let ty = self.data[ty].clone();
                tokens.extend(ty.1);
                next.extend(ty.2.into_iter().filter(|id| !completed.contains(id)));
            }

            todo = next;
        }

        let root_ty = root.0;
        tokens.extend(quote! {
            pub type Ast = #root_ty;

            pub enum Error {
                BadBranch(char, &'static [char]),
                EndOfStream,
            }

            pub type Result<T> = std::result::Result<T, Error>;

            pub trait Parser: Iterator<Item = char> {
                fn peek(&mut self) -> Option<char>;

                fn parse<T: Parse>(&mut self) -> Result<T> where Self: Sized {
                    T::parse(self)
                }

                fn take_str(&mut self, s: &str) -> Result<()>;

                fn error<T>(&mut self, e: Error) -> Result<T>;
            }

            pub trait Parse: Sized {
                fn parse<P: Parser>(input: &mut P) -> Result<Self>;
            }

        });

        // Good enough guess of whether we need concatenation rules
        if !self.cat_map.is_empty() {
            tokens.extend(quote! {
                impl<A: Parse, B: Parse> Parse for (A, B) {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        let a = input.parse()?;
                        let b = input.parse()?;
                        Ok((a, b))
                    }
                }
            });
        }

        // Good enough guess of whether we need variable rules
        if !self.fix_map.is_empty() {
            tokens.extend(quote! {
                impl<T: Parse + Sized> Parse for Box<T> {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        input.parse().map(Box::new)
                    }
                }
            });
        }

        tokens
    }
}
