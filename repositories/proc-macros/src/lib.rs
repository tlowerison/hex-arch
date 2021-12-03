extern crate proc_macro;

use proc_macro::TokenStream;
use repositories_core::RepositoriesInput;
use syn::parse_macro_input;

#[proc_macro]
pub fn repositories(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories_core::repositories(input, false).into()
}

#[proc_macro]
pub fn print_repositories(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories_core::repositories(input, true).into()
}
