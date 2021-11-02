#[macro_use] extern crate quote;
extern crate proc_macro;

use common::*;
use proc_macro::TokenStream;
use syn::{Attribute, PathArguments};

fn is_matching_attr(attr: &Attribute) -> bool {
    let path = &attr.path;
    if let Some(_) = path.leading_colon {
        return false;
    }
    if path.segments.len() != 1 {
        return false;
    }
    let first_segment = path.segments.first().unwrap();
    if let PathArguments::None = first_segment.arguments {
        if format!("{}", first_segment.ident) == "try_new_default" {
            return true;
        }
    }
    false
}

#[proc_macro_derive(TryNew, attributes(try_new_default))]
pub fn derive_try_new(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;
    let error_name = format_ident!("{}Error", name);

    let field_name_type_and_inclusion_in_signatures = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let mut include_in_signature = true;
                    for attr in field.attrs.iter() {
                        if is_matching_attr(&attr) {
                            include_in_signature = false;
                        }
                    }
                    (field.ident.as_ref().unwrap().clone(), field.ty.clone(), include_in_signature)
                })
                .collect(),
            syn::Fields::Unnamed(fields_unnamed) => fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    let mut include_in_signature = true;
                    for attr in field.attrs.iter() {
                        if is_matching_attr(&attr) {
                            include_in_signature = false;
                        }
                    }
                    (format_ident!("_{}", index), field.ty.clone(), include_in_signature)
                })
                .collect(),
            syn::Fields::Unit => vec![],
        },
        _ => panic!("TryNew can only be derived on struct data types at this time."),
    };

    let (signature_field_names, signature_field_types) = transpose_2(field_name_type_and_inclusion_in_signatures
        .iter()
        .filter_map(|(field_name, field_type, inclusions_in_signature)|
            if *inclusions_in_signature {
                Some((field_name.clone(), field_type.clone()))
            } else {
                None
            }
        )
        .collect()
    );

    let (instance_field_names, instance_field_values) = transpose_2(field_name_type_and_inclusion_in_signatures
        .iter()
        .filter_map(|(field_name, field_type, inclusions_in_signature)|
            if *inclusions_in_signature {
                Some((field_name.clone(), quote! { #field_name }))
            } else {
                Some((field_name.clone(), quote! { #field_type::default() }))
            }
        )
        .collect()
    );

    let instance = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(_) => quote! { #name { #(#instance_field_names: #instance_field_values,)* } },
            syn::Fields::Unnamed(_) => quote! { #name(#(#instance_field_values,)*) },
            syn::Fields::Unit => quote! { #name },
        },
        _ => unreachable!(),
    };

    let gen = quote! {
        impl #name {
            pub fn try_new(#(#signature_field_names: #signature_field_types),*) -> Result<#name, #error_name> {
                #name::validate(#instance)
            }
        }
    };
    gen.into()
}
