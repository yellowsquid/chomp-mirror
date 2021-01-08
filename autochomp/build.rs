use std::{
    env,
    error::Error,
    fmt::Display,
    fs,
    io::{Read, Write},
    path::Path,
    process::Command,
};

use chomp::{
    chomp::{
        check::{InlineCall, TypeCheck},
        context::Context,
        visit::Visitable,
    },
    lower::{rust::RustBackend, Backend, GenerateCode},
    nibble::cst::File,
};

const PATH: &str = "src/nibble.nb";

#[derive(Debug)]
struct UndecVar;

impl Display for UndecVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Undeclared variable somewhere.")
    }
}
impl Error for UndecVar {}

fn main() {
    println!("cargo:rerun-if-changed={}", PATH);

    let out_dir = env::var("OUT_DIR").unwrap();

    let mut input = String::new();

    fs::File::open(PATH)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|mut file| {
            file.read_to_string(&mut input)
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|_| syn::parse_str(&input).map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|nibble: File| nibble.convert().ok_or(Box::new(UndecVar) as Box<dyn Error>))
        .and_then(|(funs, goal)| {
            funs.into_iter()
                .try_rfold(goal, |goal, function| {
                    goal.fold(&mut InlineCall::new(function))
                })
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|term| {
            let mut context = Context::default();
            term.fold(&mut TypeCheck {
                context: &mut context,
            })
            .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .map(|typed| {
            let mut backend = RustBackend::default();
            let id = typed.gen(&mut backend);
            backend.emit_code(id)
        })
        .and_then(|code| {
            fs::File::create(Path::new(&out_dir).join("nibble.rs"))
                .and_then(|mut f| write!(f, "{}", code))
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .unwrap();

    Command::new("rustfmt")
        .arg(&format!("{}/nibble.rs", out_dir))
        .status()
        .unwrap();
}
