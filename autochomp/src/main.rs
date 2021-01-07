use std::{error::Error, io::{self, Read, Write}, process::exit};

use chewed::Parse;

mod nibble {
    include!(concat!(env!("OUT_DIR"), "/nibble.rs"));
}

fn main() {
    let mut input = String::new();
    let res = io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|_| nibble::Ast::parse(input.chars().peekable()).map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|ast| {
            write!(io::stdout(), "{:?}", ast).map_err(|e| Box::new(e) as Box<dyn Error>)
        });

    if let Err(e) = res {
        eprintln!("{}", e);
        exit(1)
    }
}
