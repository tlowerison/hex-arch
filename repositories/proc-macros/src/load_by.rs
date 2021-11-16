use proc_macro2::TokenStream as TokenStream2;
use syn::{braced, Field, Ident, token, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

pub (crate) struct LoadByInput {
    name_ident: Ident,
    snake_name_ident: Ident,
    _brace_token: token::Brace,
    fields: Punctuated<Field, Token![,]>,
}

impl Parse for LoadByInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(LoadByInput {
            name_ident: input.parse()?,
            snake_name_ident: input.parse()?,
            _brace_token: braced!(content in input),
            fields: content.parse_terminated(Field::parse_named)?,
        })
    }
}

pub (crate) fn load_by(input: LoadByInput) -> TokenStream2 {
    let name = input.name_ident;
    let snake_name = input.snake_name_ident;

    let field_names: Vec<_> = input.fields
        .iter()
        .map(|field| field.ident.clone().unwrap())
        .collect();

    let field_types: Vec<_> = input.fields
        .iter()
        .map(|field| field.ty.clone())
        .collect();

    let expr = quote! {
        repositories_paste! {
            pub trait [<#name LoadByFieldsRepository>]: [<#name ReadRepository>] {
                #(
                    fn [<load_ #snake_name s_by_ #field_names s>]([<#field_names s>]: Vec<#field_types>, client: Self::Client<'_>) -> Result<Vec<<Self as [<#name BaseRepository>]>::Record>, Self::Error>;
                )*
            }

            impl #name {
                #(
                    pub fn [<get_by_ #field_names>]<Adaptor: [<#name LoadByFieldsRepository>] + ReadRepository>(#field_names: #field_types) -> [<Get #name Builder>]<Adaptor> {
                        [<Get #name Builder>] {
                            adaptor: Adaptor::default(),
                            load_adaptor_record: Box::new(move |client| Ok(
                                Adaptor::[<load_ #snake_name s_by_ #field_names s>](vec![#field_names], client)?
                                    .pop()
                                    .ok_or_else(|| <Adaptor as BaseRepository>::Error::not_found())?
                            )),
                            load_relations: [<Load #name Relations>]::default(),
                        }
                    }

                    pub fn [<get_batch_by_ #field_names s>]<Adaptor: [<#name LoadByFieldsRepository>] + ReadRepository>([<#field_names s>]: Vec<#field_types>) -> [<Get #name sBuilder>]<Adaptor> {
                        [<Get #name sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: [<#field_names s>].len() as isize,
                            load_adaptor_records: Box::new(move |client| Ok(
                                Adaptor::[<load_ #snake_name s_by_ #field_names s>]([<#field_names s>], client)?
                            )),
                            load_relations: [<Load #name Relations>]::default(),
                        }
                    }
                )*
            }
        }
    };

    expr.into()
}
