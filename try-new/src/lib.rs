#[macro_use] extern crate quote;
extern crate proc_macro;

use common::transpose_2;
use proc_macro::TokenStream;
use syn::{Ident, Type};

#[proc_macro_derive(TryNew)]
pub fn try_new(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;
    let error_name = format_ident!("{}Error", name);
    let (fields, field_types) = transpose_2::<Ident, Type>(match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| (field.ident.as_ref().unwrap().clone(), field.ty.clone()))
                .collect(),
            syn::Fields::Unnamed(fields_unnamed) => fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(index, field)| (format_ident!("{}", index), field.ty.clone()))
                .collect(),
            _ => vec![],
        },
        _ => panic!("Getters can only be derived on struct data types at this time."),
    });

    let gen = quote! {
        impl #name {
            pub fn try_new(#(#fields: #field_types),*) -> Result<#name, #error_name> {
                #name::validate(#name { #(#fields: #fields,)* })
            }
        }
    };
    gen.into()
}
