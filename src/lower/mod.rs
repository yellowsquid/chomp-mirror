use std::{
    collections::{BTreeSet, HashMap},
    convert::TryInto,
};

use heck::{CamelCase, SnakeCase};
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned};
use syn::{Index, LitStr};

use crate::chomp::{Name, ast, set::FirstSet, typed::{
        lower::{Backend, GenerateCode},
        Alt, Cat, Epsilon, Fix, Literal, Type, Typed, TypedExpression, Variable,
    }};

#[derive(Clone, Debug)]
struct Ty {
    name: TokenStream,
    rest: TokenStream,
    deps: BTreeSet<usize>,
}

#[derive(Debug)]
pub struct RustBackend {
    // Indexed by ID, stores type, impl and dependencies
    data: Vec<Ty>,
    eps_id: Option<usize>,
    lit_map: HashMap<String, usize>,
    cat_map: HashMap<Vec<usize>, usize>,
    alt_map: HashMap<Vec<usize>, usize>,
    fix_map: HashMap<TypedExpression, usize>,
    var_map: HashMap<usize, usize>, // Key is fix point ID
    name_map: HashMap<Ident, usize>,
    can_char: BTreeSet<usize>,
    context: Vec<usize>,
}

type NamedSpan = (Option<Name>, Span);

impl RustBackend {
    fn new_type_name(&mut self, prefix: &str, name: Option<Name>, span: Span) -> (usize, Ident) {
        let id = self.data.len();

        match name {
            None => (id, format_ident!("{}{}", prefix, id + 1, span = span)),
            Some(name) => {
                let name = name.to_camel_case().into_ident(span);
                let count = self.name_map.entry(name.clone()).or_insert(0);
                *count += 1;
                (id, format_ident!("{}{}", name, count))
            }
        }
    }

    fn recurse_exprs<I: IntoIterator<Item = TypedExpression>>(
        &mut self,
        iter: I,
    ) -> (Vec<Type>, Vec<NamedSpan>, Vec<usize>) {
        let (ty_name_span, ids): (Vec<_>, _) = iter
            .into_iter()
            .map(|e| {
                (
                    (
                        e.get_type().clone(),
                        (e.name.clone(), e.span.unwrap_or_else(Span::call_site)),
                    ),
                    e.gen(self),
                )
            })
            .unzip();
        let (ty, name_span) = ty_name_span.into_iter().unzip();
        (ty, name_span, ids)
    }

    fn indexes<'a, I: 'a + IntoIterator<Item = Span>>(
        prefix: &'a str,
        iter: I,
    ) -> impl Iterator<Item = (Index, Ident)> + '_ {
        iter.into_iter().enumerate().map(move |(idx, span)| {
            (
                Index {
                    index: idx.try_into().unwrap(),
                    span,
                },
                format_ident!("{}{}", prefix, idx, span = span),
            )
        })
    }

    fn ty_names<'a, I: 'a + IntoIterator<Item = usize>>(
        &'a self,
        iter: I,
    ) -> impl Iterator<Item = TokenStream> + '_ {
        iter.into_iter().map(move |id| self.data[id].name.clone())
    }

    fn name_parts_snake<'a, I: 'a + IntoIterator<Item = NamedSpan>>(
        default: &'a str,
        iter: I,
    ) -> impl Iterator<Item = Ident> + '_ {
        let mut name_map = HashMap::new();
        iter.into_iter()
            .enumerate()
            .map(move |(index, (name, span))| match name {
                None => format_ident!("{}{}", default, index + 1, span = span),
                Some(name) => {
                    let name = name.to_snake_case().into_ident(span);
                    let count = name_map.entry(name.clone()).or_insert(0_usize);
                    *count += 1;
                    format_ident!("{}{}", name, count)
                }
            })
    }

    fn name_parts_camel<'a, I: 'a + IntoIterator<Item = NamedSpan>>(
        default: &'a str,
        iter: I,
    ) -> impl Iterator<Item = Ident> + '_ {
        let mut name_map = HashMap::new();
        iter.into_iter()
            .enumerate()
            .map(move |(index, (name, span))| match name {
                None => format_ident!("{}{}", default, index + 1, span = span),
                Some(name) => {
                    let name = name.to_camel_case().into_ident(span);
                    let count = name_map.entry(name.clone()).or_insert(0_usize);
                    *count += 1;
                    format_ident!("{}{}", name, count)
                }
            })
    }
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
            name_map: HashMap::new(),
            can_char: BTreeSet::new(),
            context: Vec::new(),
        }
    }
}

impl Backend for RustBackend {
    type Id = usize;

    type Code = TokenStream;

    fn gen_epsilon(&mut self, _name: Option<Name>, _span: Option<Span>, _eps: Epsilon) -> Self::Id {
        if let Some(id) = self.eps_id {
            return id;
        }

        let id = self.data.len();
        self.data.push(Ty {
            name: quote! {Epsilon},
            rest: TokenStream::new(),
            deps: BTreeSet::new(),
        });
        id
    }

    fn gen_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Id {
        let span = span.unwrap_or_else(Span::call_site);

        let lit: ast::Literal = lit.into();
        if let Some(&id) = self.lit_map.get(&lit) {
            return id;
        }

        let (id, name) = self.new_type_name("Lit", name, span);
        let doc_name = format!(r#"The literal string `{:?}`."#, lit);
        let lit = LitStr::new(&lit, span);

        let mut rest = quote_spanned! {span=>
            #[doc=#doc_name]
            #[derive(Clone, Debug, Eq, Hash, PartialEq)]
            pub struct #name;

            impl ::std::fmt::Display for #name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    write!(f, "{}", #lit)
                }
            }

            impl Parse for #name {
                fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                    input.consume_str(#lit).map(|()| #name)
                }
            }
        };

        if let [c] = lit.value().chars().collect::<Vec<_>>()[..] {
            self.can_char.insert(id);
            rest.extend(quote_spanned! {span=>
                                        impl From<#name> for char {
                                            fn from(_: #name) -> Self {
                                                #c
                                            }
                                        }
            });
        }

        self.lit_map.insert(lit.value(), id);
        self.data.push(Ty {
            name: TokenTree::from(name).into(),
            rest,
            deps: BTreeSet::new(),
        });

        id
    }

    fn gen_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Id {
        let span = span.unwrap_or_else(Span::call_site);
        let (_, name_spans, ids) = self.recurse_exprs(cat);

        if let Some(&id) = self.cat_map.get(&ids) {
            return id;
        }

        let (id, name) = self.new_type_name("Cat", name, span);
        let ty_names: Vec<_> = self.ty_names(ids.iter().copied()).collect();
        let named = name_spans.iter().any(|(name, _)| name.is_some());
        let (indexes, number_parts): (Vec<_>, Vec<_>) =
            Self::indexes("part", name_spans.iter().cloned().map(|(_, span)| span)).unzip();
        let name_parts: Vec<_> = Self::name_parts_snake("part", name_spans).collect();
        let rest = if named {
            quote_spanned! {span=>
                #[derive(Clone, Debug, Eq, Hash, PartialEq)]
                pub struct #name {
                    #(pub #name_parts: #ty_names),*
                }

                impl ::std::fmt::Display for #name {
                    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                        #(self.#name_parts.fmt(f)?;)*
                        Ok(())
                    }
                }

                impl Parse for #name {
                    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                        #(let #number_parts = input.take()?;)*
                        Ok(Self {#(#name_parts: #number_parts),*})
                    }
                }
            }
        } else {
            quote_spanned! {span=>
                #[derive(Clone, Debug, Eq, Hash, PartialEq)]
                pub struct #name(#(pub #ty_names),*);

                impl ::std::fmt::Display for #name {
                    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                        #(self.#indexes.fmt(f)?;)*
                        Ok(())
                    }
                }

                impl Parse for #name {
                    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                        #(let #number_parts = input.take()?;)*
                        Ok(Self(#(#number_parts),*))
                    }
                }
            }
        };

        self.cat_map.insert(ids.clone(), id);
        self.data.push(Ty {
            name: TokenTree::from(name).into(),
            rest,
            deps: ids.into_iter().collect(),
        });
        id
    }

    fn gen_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Id {
        let span = span.unwrap_or_else(Span::call_site);
        let (tys, name_spans, ids) = self.recurse_exprs(alt);

        if let Some(&id) = self.alt_map.get(&ids) {
            return id;
        }

        let (id, name) = self.new_type_name("Alt", name, span);
        let ty_names: Vec<_> = self.ty_names(ids.iter().copied()).collect();
        let name_parts: Vec<_> = Self::name_parts_camel("Branch", name_spans).collect();

        let nullable = tys
            .iter()
            .enumerate()
            .find(|(_, ty)| ty.nullable())
            .map(|(idx, _)| name_parts[idx].clone());
        let (first_alts, firsts): (Vec<_>, Vec<_>) = tys
            .iter()
            .map(Type::first_set)
            .cloned()
            .enumerate()
            .filter(|(_, fs)| !fs.is_empty())
            .map(|(idx, fs)| {
                let fs = fs.into_iter();
                (name_parts[idx].clone(), quote! {#(Some(#fs))|*})
            })
            .unzip();
        let all_firsts = tys
            .iter()
            .map(Type::first_set)
            .cloned()
            .flat_map(FirstSet::into_iter);

        let mut rest = quote_spanned! {span=>
            #[derive(Clone, Debug, Eq, Hash, PartialEq)]
            pub enum #name {
                #(#name_parts(#ty_names)),*
            }

            impl ::std::fmt::Display for #name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    match self {
                        #(Self::#name_parts(inner) => inner.fmt(f)),*
                    }
                }
            }
        };

        rest.extend(if let Some(nullable) = nullable {
                quote_spanned! {span=>
                    impl Parse for #name {
                        fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                            match input.peek() {
                                #(#firsts => Ok(Self::#first_alts(input.take()?)),)*
                                _ => Ok(Self::#nullable(input.take()?))
                            }
                        }
                    }
                }
            } else {
                quote_spanned! {span=>
                    impl Parse for #name {
                        fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                            match input.peek() {
                                #(#firsts => Ok(Self::#first_alts(input.take()?)),)*
                                Some(c) => Err(TakeError::BadBranch(input.pos(), c, &[#(#all_firsts),*])),
                                None => Err(TakeError::EndOfStream(input.pos()))
                            }
                        }
                    }
                }
            });

        if ids.iter().all(|id| self.can_char.contains(id)) {
            self.can_char.insert(id);
            rest.extend(quote_spanned! {span=>
                impl From<#name> for char {
                    fn from(alt: #name) -> Self {
                        match alt {
                            #(#name::#name_parts(inner) => inner.into()),*
                        }
                    }
                 }
            })
        }

        self.alt_map.insert(ids.clone(), id);
        self.data.push(Ty {
            name: TokenTree::from(name).into(),
            rest,
            deps: ids.into_iter().collect(),
        });
        id
    }

    fn gen_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Id {
        let span = span.unwrap_or_else(Span::call_site);
        let inner = fix.unwrap();

        if let Some(&id) = self.fix_map.get(&inner) {
            return id;
        }

        let (id, name) = self.new_type_name("Fix", name, span);
        self.fix_map.insert(inner.clone(), id);

        self.data.push(Ty {
            name: TokenTree::from(name.clone()).into(),
            rest: TokenStream::new(),
            deps: BTreeSet::new(),
        });

        self.context.push(id);
        let inner = inner.gen(self);
        self.context.pop();

        let inner_ty = self.data[inner].name.clone();
        let rest = quote_spanned! {span=>
            #[derive(Clone, Debug, Eq, Hash, PartialEq)]
            pub struct #name(pub #inner_ty);

            impl ::std::fmt::Display for #name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    self.0.fmt(f)
                }
            }

            impl Parse for #name {
                fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
                    input.take().map(Self)
                }
            }
        };
        self.data[id].rest = rest;
        self.data[id].deps = [inner].iter().cloned().collect();
        id
    }

    fn gen_variable(&mut self, _name: Option<Name>, span: Option<Span>, var: Variable) -> Self::Id {
        let span = span.unwrap_or_else(Span::call_site);
        let fix_id = self.context[self.context.len() - var.index() - 1];

        if let Some(&id) = self.var_map.get(&fix_id) {
            return id;
        }

        let id = self.data.len();
        let fix_ty = self.data[fix_id].name.clone();
        let name = quote_spanned! {span=> Box<#fix_ty>};
        self.var_map.insert(fix_id, id);
        self.data.push(Ty {
            name,
            rest: TokenStream::new(),
            deps: BTreeSet::new(),
        });

        id
    }

    fn emit_code(self, name: Option<Name>, span: Option<Span>, id: Self::Id) -> Self::Code {
        let span = span.unwrap_or_else(Span::call_site);

        let mut tokens = quote_spanned! {span=>
            use ::chewed::*;
        };

        let mut todo = [id].iter().copied().collect::<BTreeSet<_>>();
        let mut completed = BTreeSet::new();

        while !todo.is_empty() {
            let next = todo.clone();
            completed.append(&mut todo);

            for ty in next {
                let ty = self.data[ty].clone();
                tokens.extend(ty.rest);
                todo.extend(ty.deps.into_iter().filter(|id| !completed.contains(id)));
            }
        }

        let root_name = self.data[id].name.clone();

        tokens.extend(if let Some(name) = name {
            let name = name.into_ident(span);
            let count = self.name_map.get(&name).copied().unwrap_or_default() + 1;
            let name = format_ident!("{}{}", name, count);
            quote_spanned! {span=>
                pub type #name = #root_name;
            }
        } else {
            quote_spanned! {span=>
                pub type Ast = #root_name;
            }
        });

        tokens
    }
}
