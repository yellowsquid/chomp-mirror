mod check;
mod closed;
mod deepen;
mod infer;
mod inline;
mod shallow;
mod spanning;
mod substitute;

pub use check::TypeCheck;
pub use closed::Closed;
pub use deepen::DeepenVars;
pub use infer::TypeInfer;
pub use inline::InlineCalls;
pub use shallow::ShallowVars;
pub use spanning::Spanning;
pub use substitute::SubstituteParams;
