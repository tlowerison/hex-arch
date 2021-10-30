#[macro_use] extern crate quote;
extern crate proc_macro;

use common::transpose_3;
use proc_macro::TokenStream;
use syn::{Ident, Type};

#[proc_macro_derive(Getters)]
pub fn getters(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;
    let name_with_pub_fields = format_ident!("{}Pub", name);

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

    let struct_with_pub_fields = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(_) => quote! {
                pub struct #name_with_pub_fields {
                    #(pub #fields: #field_types,)*
                }
            },
            syn::Fields::Unnamed(_) => quote! {
                pub struct #name_with_pub_fields(
                    #(pub #field_types,)*
                );
            },
            syn::Fields::Unit => quote! {
                pub struct #name_with_pub_fields;
            },
        },
        _ => unreachable!(),
    };

    let impl_from_struct_for_struct_with_pub_fields = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(_) => quote! {
                impl From<#name> for #name_with_pub_fields {
                    fn from(value: #name) -> #name_with_pub_fields {
                        #name_with_pub_fields {
                            #(#fields: value.#fields,)*
                        }
                    }
                }
            },
            syn::Fields::Unnamed(_) => quote! {
                impl From<#name> for #name_with_pub_fields {
                    fn from(value: #name) -> #name_with_pub_fields {
                        #name_with_pub_fields(
                            #(value.#fields,)*
                        )
                    }
                }
            },
            syn::Fields::Unit => quote! {
                impl From<#name> for #name_with_pub_fields {
                    fn from(_: #name) -> #name_with_pub_fields {
                        #name_with_pub_fields
                    }
                }
            },
        },
        _ => unreachable!(),
    };

    let gen = quote! {
        impl #name {
            #(
                pub fn #getter_fn_names(&self) -> &#field_types {
                    &self.#fields
                }
            )*
        }

        #struct_with_pub_fields

        #impl_from_struct_for_struct_with_pub_fields
    };
    gen.into()
}
