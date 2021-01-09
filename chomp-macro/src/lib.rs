use chomp::{
    chomp::{
        check::{InlineCalls, TypeCheck},
        context::Context,
        visit::Visitable,
    },
    lower::{rust::RustBackend, Backend, GenerateCode},
    nibble::cst::File,
};
use proc_macro::{Span, TokenStream};
use syn::Error;

#[proc_macro]
pub fn nibble(item: TokenStream) -> TokenStream {
    syn::parse(item)
        .and_then(|nibble: File| {
            nibble
                .convert()
                .ok_or_else(|| todo!())
        })
        .and_then(|(funs, goal)| {
            funs.into_iter()
                .try_rfold(goal, |goal, function| {
                    goal.fold(&mut InlineCalls::new(function))
                })
                .map_err(Error::from)
        })
        .and_then(|expr| {
            let mut context = Context::default();
            expr.fold(&mut TypeCheck {
                context: &mut context,
            })
            .map_err(Error::from)
        })
        .map(|typed| {
            let mut backend = RustBackend::default();
            let id = typed.gen(&mut backend);
            backend.emit_code(id)
        })
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
