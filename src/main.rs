use std::{
    error::Error,
    io::{self, Read, Write},
    process::exit,
};

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
    nibble::File,
};

fn main() {
    let mut input = String::new();
    let res = io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|_| syn::parse_str(&input).map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|nibble: File| nibble.convert().map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|(funs, goal)| {
            funs.into_iter()
                .try_rfold(goal, |goal, function| {
                    goal.fold(&mut InlineCalls { function })
                })
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|term| {
            let mut context = Context::default();
            term.fold(&mut TypeInfer {
                context: &mut context,
            })
            .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .map(|typed| {
            let mut backend = RustBackend::default();
            let id = typed.gen(&mut backend);
            backend.emit_code(None, None, id)
        })
        .and_then(|code| {
            write!(io::stdout(), "{:#}", code).map_err(|e| Box::new(e) as Box<dyn Error>)
        });

    if let Err(e) = res {
        eprintln!("{}", e);
        exit(1)
    }
}
