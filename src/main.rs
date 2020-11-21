use chomp::{
    ast::{
        convert::{Context, Convert},
        typed::Type,
    },
    nibble::Expression,
};
use proc_macro2::Span;
use std::io::{self, Error, ErrorKind, Read, Write};
use syn::Ident;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let nibble: Expression =
        syn::parse_str(&input).map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
    let term = nibble.convert(&Context::new());
    // FIXME: better error handling here
    let typed = term.well_typed(&mut Vec::new()).unwrap();
    let code = typed.emit_code(Ident::new("Ast", Span::call_site()));
    write!(io::stdout(), "{:#}", code)
}
