#[macro_use] extern crate quote;
extern crate proc_macro;

use common::*;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Ident, PathArguments, Type};

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
        if format!("{}", first_segment.ident) == "generate_valid" {
            return true;
        }
    }
    false
}

fn named_fields_util(fields_named: &syn::FieldsNamed, draft_instance_name: &Ident, should_access_draft_instance: bool) -> ((Vec<Ident>, Vec<TokenStream2>), (Vec<Ident>, Vec<Type>)) {
    let field_name_type_and_inclusion_in_drafts: Vec<_> = fields_named
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
        .collect();

    let (field_names, field_instances) = transpose_2(field_name_type_and_inclusion_in_drafts
        .iter()
        .map(|(field_name, field_type, inclusion_in_draft)| (
            field_name.clone(),
            if *inclusion_in_draft {
                if should_access_draft_instance {
                    quote! { #draft_instance_name.#field_name }
                } else {
                    quote! { #field_name }
                }
            } else {
                quote! { <#field_type>::default() }
            }
        ))
        .collect()
    );

    let (draft_field_names, draft_field_types) = transpose_2(field_name_type_and_inclusion_in_drafts
        .iter()
        .filter_map(|(field_name, field_type, inclusion_in_draft)|
            if *inclusion_in_draft {
                Some((field_name.clone(), field_type.clone()))
            } else {
                None
            }
        )
        .collect()
    );

    (
        (field_names, field_instances),
        (draft_field_names, draft_field_types),
    )
}

fn unnamed_fields_util(fields_unnamed: &syn::FieldsUnnamed) -> (Vec<Type>, Vec<usize>) {
    let field_types: Vec<_> = fields_unnamed
        .unnamed
        .iter()
        .map(|field| {
            for attr in field.attrs.iter() {
                if is_matching_attr(&attr) {
                    panic!("generate_valid attribute cannot be attached to unnamed fields");
                }
            }
            field.ty.clone()
        })
        .collect();

    let indices: Vec<_> = (0..field_types.len()).collect();

    (field_types, indices)
}

#[proc_macro_derive(Validated, attributes(generate_valid))]
pub fn validated(item: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(item).unwrap();

    let name = &ast.ident;
    let draft_name = format_ident!("Draft{}", name);
    let error_name = format_ident!("{}Error", name);
    let instance_name = format_ident!("instance");
    let draft_instance_name = format_ident!("draft");

    let gen = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => {
                let (
                    (field_names, field_instances),
                    (draft_field_names, draft_field_types),
                ) = named_fields_util(&fields_named, &draft_instance_name, true);

                quote! {
                    pub struct #draft_name {
                        #(pub #draft_field_names: #draft_field_types),*
                    }

                    impl From<#name> for #draft_name {
                        fn from(#instance_name: #name) -> #draft_name {
                            #draft_name {
                                #(#draft_field_names: #instance_name.#draft_field_names),*
                            }
                        }
                    }

                    impl TryFrom<#draft_name> for #name {
                        type Error = #error_name;

                        fn try_from(#draft_instance_name: #draft_name) -> Result<Self, Self::Error> {
                            #name::validate(#name {
                                #(#field_names: #field_instances),*
                            })
                        }
                    }
                }
            },
            syn::Fields::Unnamed(fields_unnamed) => {
                let (field_types, indices) = unnamed_fields_util(&fields_unnamed);
                quote! {
                    pub struct #draft_name(
                        #(pub #field_types),*
                    );

                    impl From<#name> for #draft_name {
                        fn from(#instance_name: #name) -> #draft_name {
                            #draft_name(
                                #(#instance_name.#indices),*
                            )
                        }
                    }

                    impl TryFrom<#draft_name> for #name {
                        type Error = #error_name;

                        fn try_from(draft: #draft_name) -> Result<Self, Self::Error> {
                            #name::validate(#name(
                                #(draft.#indices),*
                            ))
                        }
                    }
                }
            },
            syn::Fields::Unit => {
                quote! {
                    pub struct #draft_name;

                    impl From<#name> for #draft_name {
                        fn from(#instance_name: #name) -> #draft_name {
                            #draft_name
                        }
                    }

                    impl TryFrom<#draft_name> for #name {
                        type Error = #error_name;

                        fn try_from(_: #draft_name) -> Result<Self, Self::Error> {
                            #name::validate(#name)
                        }
                    }
                }
            },
        },
        syn::Data::Enum(data_enum) => {
            let (variant_idents, variant_tokens) = transpose_2(data_enum.variants
                .iter()
                .map(|variant| (
                    variant.ident.clone(),
                    match &variant.fields {
                        syn::Fields::Named(fields_named) => {
                            let (
                                (field_names, field_instances),
                                (draft_field_names, draft_field_types),
                            ) = named_fields_util(&fields_named, &draft_instance_name, false);
                            (
                                quote! { { #(#draft_field_names: #draft_field_types,)* } },
                                quote! { { #(#draft_field_names,)* } },
                                quote! { { #(#draft_field_names: #draft_field_names,)* } },
                                quote! { { #(#draft_field_names,)* .. } },
                                quote! { { #(#field_names: #field_instances,)* } },
                            )
                        },
                        syn::Fields::Unnamed(fields_unnamed) => {
                            let (field_types, indices) = unnamed_fields_util(&fields_unnamed);
                            let field_names: Vec<_> = indices.into_iter().map(|index| format_ident!("_{}", index)).collect();
                            (
                                quote! { ( #(#field_types,)* ) },
                                quote! { ( #(#field_names,)* ) },
                                quote! { ( #(#field_names,)* ) },
                                quote! { ( #(#field_names,)* .. ) },
                                quote! { ( #(#field_names,)* ) },
                            )
                        },
                        syn::Fields::Unit => (quote! {}, quote! {}, quote! {}, quote! {}, quote! {}),
                    },
                ))
                .collect()
            );
            let (draft_defs, draft_expansions, draft_instances, expansions, instances) = transpose_5(variant_tokens);
            quote! {
                pub enum #draft_name {
                    #(#variant_idents #draft_defs),*
                }

                impl From<#name> for #draft_name {
                    fn from(#instance_name: #name) -> #draft_name {
                        match #instance_name {
                            #(#name::#variant_idents #expansions => #draft_name::#variant_idents #draft_instances),*
                        }
                    }
                }

                impl TryFrom<#draft_name> for #name {
                    type Error = #error_name;

                    fn try_from(#draft_instance_name: #draft_name) -> Result<Self, Self::Error> {
                        #name::validate(match #draft_instance_name {
                            #(#draft_name::#variant_idents #draft_expansions => #name::#variant_idents #instances),*
                        })
                    }
                }
            }
        },
        syn::Data::Union(_) => panic!("TryNew is not supported for union data types"),
    };

    gen.into()
}
