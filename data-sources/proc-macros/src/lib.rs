#![feature(proc_macro_span)]

#[macro_use] extern crate quote;
extern crate proc_macro;

use data_sources_core::AdaptorInput;
use proc_macro::{Span, TokenStream};
use syn::{DeriveInput, Ident, parse_macro_input};

#[proc_macro]
pub fn adaptor(item: TokenStream) -> TokenStream  {
    let input = parse_macro_input!(item as AdaptorInput);
    data_sources_core::adaptor(input, Span::call_site().source_file().path()).into()
}

#[proc_macro_derive(IsChangeset)]
pub fn derive_is_changeset(tokens: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(tokens).unwrap();

    let ident = &ast.ident;

    let primary_key = get_primary_key(&ast);
    let field_names = get_field_names(&ast, &primary_key);

    let tokens = quote! {
        impl hex_arch::IsChangeset for #ident {
            fn is_changeset(&self) -> bool {
                !(#(self.#field_names == None) && *)
            }
        }
    };

    tokens.into()
}

fn get_primary_key(ast: &DeriveInput) -> Ident {
    ast.attrs
        .iter()
        .find(|attr| {
            if let Some(attr_ident) = attr.path.get_ident() {
                if &*format!("{}", attr_ident) == "primary_key" {
                    return true
                }
            }
            false
        })
        .map(|attr| attr.parse_args().unwrap())
        .unwrap_or_else(|| format_ident!("id"))
}

fn get_field_names(ast: &DeriveInput, primary_key: &Ident) -> Vec<Ident> {
    match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .filter_map(|field| {
                    let field_name = field.ident.as_ref().unwrap().clone();
                    if field_name == *primary_key {
                        None
                    } else {
                        Some(field_name)
                    }
                })
                .collect(),
            _ => panic!("IsChangeset can only be derived on struct data types with named fields at this time."),
        },
        _ => panic!("IsChangeset can only be derived on struct data types at this time."),
    }
}
