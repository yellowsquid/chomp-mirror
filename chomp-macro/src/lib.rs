use chomp::{
    chomp::{
        ast::substitute::InlineCalls,
        typed::{
            context::Context,
            lower::{Backend, GenerateCode},
            TypeInfer,
        },
        visit::Visitable,
    },
    lower::RustBackend,
    nibble::cst::File,
};
use proc_macro::TokenStream;
use syn::Error;

#[proc_macro]
pub fn nibble(item: TokenStream) -> TokenStream {
    syn::parse(item)
        .and_then(|nibble: File| nibble.convert().map_err(Error::from))
        .and_then(|(funs, goal)| {
            funs.into_iter()
                .try_rfold(goal, |goal, function| {
                    goal.fold(&mut InlineCalls { function })
                })
                .map_err(Error::from)
        })
        .and_then(|expr| {
            let mut context = Context::default();
            expr.fold(&mut TypeInfer {
                context: &mut context,
            })
            .map_err(Error::from)
        })
        .map(|typed| {
            let mut backend = RustBackend::default();
            let id = typed.gen(&mut backend);
            backend.emit_code(None, None, id)
        })
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
