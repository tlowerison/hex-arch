#[macro_use] extern crate optional_fields;
#[macro_use] extern crate paste;
extern crate proc_macro;
#[macro_use] extern crate quote;

mod input;
mod repositories;

use input::RepositoriesInput;

use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn repositories(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories::repositories(input, false).into()
}
