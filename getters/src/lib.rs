#[macro_use] extern crate quote;
extern crate proc_macro;

use common::transpose_3;
use proc_macro::TokenStream;
use syn::{Ident, Type};

#[proc_macro_derive(Getters)]
pub fn getters(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;

    let (getter_fn_names, fields, field_types) = transpose_3::<Ident, Ident, Type>(match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let ident = field.ident.as_ref().unwrap().clone();
                    (ident.clone(), ident, field.ty.clone())
                })
                .collect(),
            syn::Fields::Unnamed(fields_unnamed) => fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(index, field)| (format_ident!("_{}", index), format_ident!("{}", index), field.ty.clone()))
                .collect(),
            syn::Fields::Unit => vec![],
        },
        _ => panic!("Getters can only be derived on struct data types at this time."),
    });

    let gen = quote! {
        impl #name {
            #(
                pub fn #getter_fn_names(&self) -> &#field_types {
                    &self.#fields
                }
            )*
        }
    };
    gen.into()
}
