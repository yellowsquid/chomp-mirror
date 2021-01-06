pub mod convert;
pub mod cst;
// impl File {
//     /// Returns function list and the goal. The function list consists of an
//     /// [`Ident`], the converted [`ast::Expression`] and the number of arguments.
//     pub fn convert(self) -> (Vec<(Ident, ast::Expression, usize)>, ast::Expression) {
//         let mut context = Context::new();
//         let map = self
//             .lets
//             .into_iter()
//             .map(|stmt| {
//                 let count = stmt.args.as_ref().map(ArgList::len).unwrap_or_default();
//                 context.set_variables(
//                     stmt.args
//                         .into_iter()
//                         .flat_map(|args| args.into_iter().map(|arg| arg.to_string())),
//                 );
//                 (stmt.name, stmt.expr.convert(&mut context), count)
//             })
//             .collect();
//         let goal = self.goal.expr.convert(&mut context);
//         (map, goal)
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::{Epsilon, Ident, Literal};
//     use syn::parse_str;

//     #[test]
//     fn parse_epsilon() {
//         assert!(parse_str::<Epsilon>("_").is_ok());
//     }

//     #[test]
//     fn parse_ident() {
//         assert_eq!(parse_str::<Ident>("x").unwrap().to_string(), "x");
//         assert_eq!(parse_str::<Ident>("x_yz").unwrap().to_string(), "x_yz");
//         assert_eq!(parse_str::<Ident>("a123").unwrap().to_string(), "a123");
//         assert_eq!(parse_str::<Ident>("𝒢𝒢").unwrap().to_string(), "𝒢𝒢");
//         assert!(parse_str::<Ident>("1").is_err());
//         assert!(parse_str::<Ident>("_").is_err());
//     }

//     #[test]
//     fn parse_literal() {
//         assert_eq!(parse_str::<Literal>(r#""hello""#).unwrap().value(), "hello")
//     }
// }
