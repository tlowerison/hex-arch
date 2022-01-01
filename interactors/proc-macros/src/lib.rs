#[macro_use] extern crate quote;
extern crate proc_macro;

use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Field, Ident, PathArguments, Type};

const GRAPHQL_LOAD_META: &'static str = "graphql_load";

fn graphql_load_meta(field: &Field) -> Option<(&Field, Ident)> {
    for attr in field.attrs.iter() {
        if let Some(path_ident) = attr.path.get_ident() {
            if format!("{}", path_ident) == GRAPHQL_LOAD_META {
                let entity_relation_snake: Ident = attr.parse_args().expect("`graphql_load` attribute expects a single ident for the relation name this field represents");
                return Some((field, entity_relation_snake));
            }
        }
    }
    None
}

fn entity_inner_ty(ty: Type) -> Type {
    match ty {
        Type::Array(type_array) => entity_inner_ty(*type_array.elem),
        Type::Paren(type_paren) => entity_inner_ty(*type_paren.elem),
        Type::Path(type_path) => {
            match type_path.path.segments.clone().into_iter().last().unwrap().arguments {
                PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                    let mut ty: Option<Type> = None;
                    let mut ty_arg_count = 0;
                    for arg in angle_bracketed_generic_arguments.args.into_iter() {
                        match arg {
                            syn::GenericArgument::Type(arg_ty) => {
                                ty = Some(arg_ty);
                                ty_arg_count += 1;
                            },
                            _ => {},
                        }
                    }

                    if ty_arg_count != 1 {
                        panic!("`graphql_load` attribute cannot be placed on types with less or more than one generic type parameters (i.e. excluding lifetime parameters, etc.)");
                    }
                    entity_inner_ty(ty.unwrap())
                },
                _ => Type::Path(type_path),
            }
        },
        Type::Reference(type_reference) => entity_inner_ty(*type_reference.elem),
        Type::Slice(type_slice) => entity_inner_ty(*type_slice.elem),
        _ => panic!("`graphql_load` cannot infer relation type"),
    }
}

#[proc_macro_derive(GraphQLLoad, attributes(graphql_load))]
pub fn derive_graphql_load(tokens: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(tokens).unwrap();
    let ident = &ast.ident;

    let (field_names, field_inner_entity_tys, field_entity_relation_snakes): (Vec<_>, Vec<_>, Vec<_>) = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .filter_map(graphql_load_meta)
                .map(|(field, entity_relation_snake)| (
                    field.ident.as_ref().unwrap().clone(),
                    entity_inner_ty(field.ty.clone()),
                    entity_relation_snake,
                ))
                .multiunzip(),
            _ => panic!("GraphQLLoad can only be derived on struct data types with named fields at this time."),
        },
        _ => panic!("GraphQLLoad can only be derived on struct data types at this time."),
    };

    let stringified_field_names: Vec<TokenStream2> = field_names.clone().into_iter().map(|x| format!("\"{}\"", x).parse().unwrap()).collect();

    let tokens = quote! {
        interactors_paste! {
            impl GraphQLLoad for #ident {
                type LoadRelations = entities::[<Load #ident Relations>];
                fn load<'a, S: 'a>(selection: &juniper::LookAheadSelection<'a, S>) -> Box<dyn Send + Fn(Self::LoadRelations) -> Self::LoadRelations> {
                    use interactors_convert_case::Casing;
                    use interactors_juniper::LookAheadMethods;

                    #( let mut [<load_in_ #field_entity_relation_snakes>]: Option<Box<dyn Send + Fn(<#field_inner_entity_tys as GraphQLLoad>::LoadRelations) -> <#field_inner_entity_tys as GraphQLLoad>::LoadRelations>> = None; )*
                    for child in selection.children().iter() {
                        match &*child.field_name().to_case(interactors_convert_case::Case::Snake) {
                            #(
                                #stringified_field_names => [<load_in_ #field_entity_relation_snakes>] = Some(#field_inner_entity_tys::load(child)),
                            )*
                            _ => {},
                        }
                    }
                    Box::new(move |mut loader| {
                        #(
                            if let Some([<load_in_ #field_entity_relation_snakes>]) = [<load_in_ #field_entity_relation_snakes>].as_ref() {
                                loader = loader.[<load_ #field_entity_relation_snakes _with>]([<load_in_ #field_entity_relation_snakes>]);
                            }
                        )*
                        loader
                    })
                }
            }
        }
    };

    tokens.into()
}
