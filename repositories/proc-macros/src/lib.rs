extern crate proc_macro;
#[macro_use] extern crate quote;

mod input;
mod load_by;
mod repositories;

use load_by::LoadByInput;
use input::RepositoriesInput;

use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn repositories(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as RepositoriesInput);
    repositories::repositories(input, false).into()
}

#[proc_macro]
pub fn load_by(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as LoadByInput);
    load_by::load_by(input).into()
}
