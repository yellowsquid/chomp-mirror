//! A typed, algebraic parser generator.
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(anonymous_parameters)]
#![warn(deprecated_in_future)]
#![warn(elided_lifetimes_in_paths)]
#![warn(keyword_idents)]
#![warn(macro_use_extern_crate)]
#![warn(meta_variable_misuse)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![allow(missing_docs)]
#![warn(missing_doc_code_examples)]
#![warn(non_ascii_idents)]
#![warn(private_doc_tests)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unaligned_references)]
#![warn(unreachable_pub)]
#![warn(unused_crate_dependencies)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]

pub mod chomp;
pub mod lower;
pub mod nibble;
