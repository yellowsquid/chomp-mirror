use chomp::{
    chomp::{
        ast::substitute::Reduce,
        typed::{
            context::Context,
            lower::{Backend, GenerateCode},
            TypeInfer,
        },
        visit::Visitable,
    },
    lower::RustBackend,
    nibble::{convert::{self, Convert}, Statement},
};
use proc_macro::{Span, TokenStream};
use syn::Error;

#[proc_macro]
pub fn nibble(item: TokenStream) -> TokenStream {
    syn::parse(item)
        .and_then(|nibble: Statement| {
            nibble
                .convert(&mut convert::Context::default())
                .map_err(Error::from)
        })
        .and_then(|expr| {
            expr.fold(&mut Reduce)
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
            backend.emit_code(None, Span::call_site().into(), id)
        })
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
