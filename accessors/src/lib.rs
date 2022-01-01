#[macro_use] extern crate quote;
extern crate proc_macro;

use itertools::Itertools;
use proc_macro::TokenStream;

#[proc_macro_derive(Getters)]
pub fn getters(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;

    let (getter_fn_names, fields, field_types): (Vec<_>, Vec<_>, Vec<_>) = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let ident = field.ident.as_ref().unwrap().clone();
                    (ident.clone(), ident, field.ty.clone())
                })
                .multiunzip(),
            syn::Fields::Unnamed(fields_unnamed) => fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(index, field)| (format_ident!("_{}", index), format_ident!("{}", index), field.ty.clone()))
                .multiunzip(),
            syn::Fields::Unit => vec![].into_iter().multiunzip(),
        },
        _ => panic!("Getters can only be derived on struct data types"),
    };

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

#[proc_macro_derive(Pub)]
pub fn pub_fn(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = ast.ident;
    let pub_name = format_ident!("Pub{}", name);

    let gen = match ast.data {
        syn::Data::Struct(data_struct) => match data_struct.fields {
            syn::Fields::Named(fields_named) => {
                let (field_names, field_types): (Vec<_>, Vec<_>) = fields_named
                    .named
                    .into_iter()
                    .map(|field| (field.ident.unwrap(), field.ty))
                    .unzip();
                quote! {
                    pub struct #pub_name {
                        #(pub #field_names: #field_types),*
                    }

                    impl From<#name> for #pub_name {
                        fn from(value: #name) -> Self {
                            #pub_name {
                                #(#field_names: value.#field_names),*
                            }
                        }
                    }
                }
            },
            syn::Fields::Unnamed(fields_unnamed) => {
                let field_types: Vec<_> = fields_unnamed
                    .unnamed
                    .into_iter()
                    .map(|field| field.ty)
                    .collect();

                let indices: Vec<_> = (0..field_types.len()).collect();
                quote! {
                    pub struct #pub_name (
                        #(pub #field_types),*
                    );

                    impl From<#name> for #pub_name {
                        fn from(value: #name) -> Self {
                            #pub_name (
                                #(value.#indices),*
                            )
                        }
                    }
                }
            },
            syn::Fields::Unit => quote! {
                pub struct #pub_name;
            },
        },
        _ => panic!("Pub can only be derived on struct data types"),
    };

    gen.into()
}
