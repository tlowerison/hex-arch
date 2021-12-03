#![feature(proc_macro_span)]

extern crate proc_macro;

use data_sources_core::AdaptorInput;
use proc_macro::{Span, TokenStream};
use syn::parse_macro_input;

#[proc_macro]
pub fn adaptor(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as AdaptorInput);
    data_sources_core::adaptor(input, Span::call_site().source_file().path()).into()
}
