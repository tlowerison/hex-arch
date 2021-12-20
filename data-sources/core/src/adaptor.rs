use crate::get_repositories_input::*;
use common::*;
use convert_case::{Case, Casing};
use proc_macro2::TokenStream as TokenStream2;
use quote::{TokenStreamExt, ToTokens};
use repositories_core::{
    *,
    read_repositories::ty_read_repository::{
        get_load_by_field_multiple_fn_name,
        load_by_field_multiple,
        load_by_multiple,
        load_by_multiple_keys,
        load_keys_by_multiple,
    },
    shared::{
        op_by_multiple_fn_name,
        op_by_multiple_keys_fn_name,
        op_keys_by_multiple_fn_name,
    },
};
use std::collections::HashMap;
use std::path::PathBuf;
use syn::{braced, Ident, LitStr, Token, parenthesized, Type};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Paren;

const PLACEHOLDER_KEY: &'static str = "_";

#[derive(Clone)]
pub struct AdaptorInput {
    pub path: LitStr,
    pub config: AdaptorConfigInput,
}

#[derive(Clone)]
pub struct AdaptorConfigInput {
    adaptor_name: Ident,
    entity_prefix: Ident,
    client: Type,
    error: Type,
    adaptor_entity_inputs: Vec<AdaptorEntityInput>,
}

#[derive(Clone)]
pub struct AdaptorEntityInput {
    ty: Ident,
    key: Ident,
    namespace: Option<Ident>,
    children: Vec<ChildInput>,
    reverse_linked_children: Vec<ChildInput>,
    load: AdaptorEntityLoadInput,
    insert: Option<AdaptorEntityMutateInput>,
    update: Option<AdaptorEntityMutateInput>,
    delete: Option<AdaptorEntityMutateInput>,
}

#[derive(Clone)]
pub struct AdaptorEntityLoadInput {
    plural: Option<TokenStream2>,
    try_plural: Option<TokenStream2>,
    all: Option<TokenStream2>,
    by: HashMap<Ident, TokenStream2>,
    keys_by: HashMap<Ident, TokenStream2>,
}

#[derive(Clone)]
pub enum AdaptorEntityMutateInput {
    FnBody(TokenStream2),
    Full,
}

#[derive(Clone)]
pub enum OrderByOrdering {
    Asc,
    Desc,
}

impl ToTokens for OrderByOrdering {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            OrderByOrdering::Asc => tokens.append(format_ident!("asc")),
            OrderByOrdering::Desc => tokens.append(format_ident!("desc")),
        }
    }
}

impl TryFrom<Ident> for OrderByOrdering {
    type Error = syn::Error;

    fn try_from(ident: Ident) -> Result<Self, Self::Error> {
        match &*format!("{}", ident).to_lowercase() {
            "asc" => Ok(OrderByOrdering::Asc),
            "desc" => Ok(OrderByOrdering::Desc),
            _ => Err(syn::Error::new_spanned(ident, "expected one of `asc` or `desc`")),
        }
    }
}

#[derive(Clone)]
pub struct ChildInput {
    pub snake: Ident,
    pub ty: Type,
    pub is_reverse_linked: bool,
    pub order_by_field: Option<Ident>,
    pub order_by_ordering: Option<OrderByOrdering>,
}

#[derive(Clone)]
pub struct LoadFnInput {
    snake_name: Ident,
    block: TokenStream2,
}

impl Parse for AdaptorInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: LitStr = input.parse()?;
        let _comma: Token![,] = input.parse()?;

        let in_brace;
        braced!(in_brace in input);
        let config_fields: AdaptorConfigInputFields = in_brace.parse()?;

        Ok(AdaptorInput {
            path,
            config: AdaptorConfigInput {
                adaptor_name: config_fields.name,
                entity_prefix: config_fields.entity_prefix,
                client: config_fields.client,
                error: config_fields.error,
                adaptor_entity_inputs: config_fields.entities.unwrap_or(vec![]),
            },
        })
    }
}

fields! {
    AdaptorConfigInput {
        name!: input -> Ident { input.parse()? },
        entity_prefix!: input -> Ident { input.parse()? },
        client!: input -> Type { input.parse()? },
        error!: input -> Type { input.parse()? },
        entities?: input -> Vec<AdaptorEntityInput> {
            let in_brace;
            braced!(in_brace in input);
            let adaptor_entity_inputs: Punctuated<AdaptorEntityInput, Token![,]> = in_brace.parse_terminated(AdaptorEntityInput::parse)?;
            adaptor_entity_inputs.into_iter().collect()
        }
    }
}

impl Parse for AdaptorEntityInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;

        let in_brace;
        braced!(in_brace in input);
        let fields: AdaptorEntityInputFields = in_brace.parse()?;

        let mut children: Vec<ChildInput> = vec![];
        let mut reverse_linked_children: Vec<ChildInput> = vec![];

        for child in fields.children.unwrap_or(vec![]).into_iter() {
            if child.is_reverse_linked {
                reverse_linked_children.push(child);
            } else {
                children.push(child);
            }
        }

        Ok(AdaptorEntityInput {
            key: format_ident!(
                "{}",
                fields.key
                    .map(|key| key.value())
                    .unwrap_or(String::from(PLACEHOLDER_KEY)),
            ),
            namespace: fields.namespace,
            children,
            reverse_linked_children,
            load: fields.load.unwrap_or_else(|| AdaptorEntityLoadInput {
                plural: None,
                try_plural: None,
                all: None,
                by: HashMap::default(),
                keys_by: HashMap::default(),
            }),
            insert: fields.insert,
            update: fields.update,
            delete: fields.delete,
            ty,
        })
    }
}

fields! {
    AdaptorEntityInput {
        namespace?: input -> Ident { input.parse()? },
        key?: input -> LitStr { input.parse()? },
        children?: input -> Vec<ChildInput> {
            let in_brace;
            braced!(in_brace in input);
            let child_inputs: Punctuated<ChildInput, Token![,]> = in_brace.parse_terminated(ChildInput::parse)?;
            child_inputs.into_iter().collect()
        },
        load?: input -> AdaptorEntityLoadInput {
            let in_brace;
            braced!(in_brace in input);
            in_brace.parse()?
        },
        insert?: input -> AdaptorEntityMutateInput { input.parse()? },
        update?: input -> AdaptorEntityMutateInput { input.parse()? },
        delete?: input -> AdaptorEntityMutateInput { input.parse()? },
    }
}

impl Parse for ChildInput {
    fn parse(input: ParseStream) -> syn::Result<ChildInput> {
        let snake: Ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;

        let is_in_parens = input.peek(Paren);
        if is_in_parens {
            let in_parens;
            parenthesized!(in_parens in input);
            ChildInput::parse_after_colon(&in_parens, snake, is_in_parens)
        } else {
            ChildInput::parse_after_colon(input, snake, is_in_parens)
        }
    }
}

impl ChildInput {
    fn parse_after_colon(input: ParseStream, snake: Ident, is_in_parens: bool) -> syn::Result<ChildInput> {
        let ty: Type = input.parse()?;

        let mut child_input = ChildInput {
            snake,
            ty,
            is_reverse_linked: false,
            order_by_field: None,
            order_by_ordering: None,
        };

        if input.peek(Token![|]) {
            let _vert: Token![|] = input.parse()?;
            let flag: Ident = input.parse()?;

            match &*format!("{}", flag) {
                "reverse_linked" => { child_input.is_reverse_linked = true; },
                _ => return Err(syn::Error::new_spanned(flag, "unrecognized flag on child entity")),
            }
        }
        if input.peek(Token![,]) {
            if !is_in_parens {
                return Ok(child_input);
            }

            let _comma: Token![,] = input.parse()?;
            child_input.order_by_field = Some(input.parse::<Ident>()?);
            child_input.order_by_ordering = input
                .parse::<Ident>()
                .ok()
                .map(OrderByOrdering::try_from)
                .transpose()?;
        }
        Ok(child_input)
    }
}

impl Parse for AdaptorEntityLoadInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fields: AdaptorEntityLoadInputFields = input.parse()?;

        Ok(AdaptorEntityLoadInput {
            plural: fields.plural,
            try_plural: fields.try_plural,
            all: fields.all,
            by: fields.by.unwrap_or(HashMap::with_capacity(0)),
            keys_by: fields.keys_by.unwrap_or(HashMap::with_capacity(0)),
        })
    }
}

fields! {
    AdaptorEntityLoadInput {
        plural?: input -> TokenStream2 {
            let in_brace;
            braced!(in_brace in input);
            in_brace.parse()?
        },
        try_plural?: input -> TokenStream2 {
            let in_brace;
            braced!(in_brace in input);
            in_brace.parse()?
        },
        all?: input -> TokenStream2 {
            let in_brace;
            braced!(in_brace in input);
            in_brace.parse()?
        },
        by?: input -> HashMap<Ident, TokenStream2> {
            let in_brace;
            braced!(in_brace in input);
            let load_fn_inputs: Punctuated<LoadFnInput, Token![,]> = in_brace.parse_terminated(LoadFnInput::parse)?;
            load_fn_inputs
                .into_iter()
                .map(|load_fn_input| (load_fn_input.snake_name, load_fn_input.block))
                .collect()
        },
        keys_by?: input -> HashMap<Ident, TokenStream2> {
            let in_brace;
            braced!(in_brace in input);
            let load_fn_inputs: Punctuated<LoadFnInput, Token![,]> = in_brace.parse_terminated(LoadFnInput::parse)?;
            load_fn_inputs
                .into_iter()
                .map(|load_fn_input| (load_fn_input.snake_name, load_fn_input.block))
                .collect()
        },
    }
}

impl Parse for LoadFnInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let snake_name: Ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;

        let in_brace;
        braced!(in_brace in input);
        let block: TokenStream2 = in_brace.parse()?;

        Ok(LoadFnInput { snake_name, block: quote! { { #block } } })
    }
}

impl Parse for AdaptorEntityMutateInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) {
            let ident: Ident = input.parse()?;
            Ok(match &*format!("{}", ident) {
                "full" => AdaptorEntityMutateInput::Full,
                _ => return Err(syn::Error::new_spanned(ident, "unrecognized identifier in insert block: expected `full`")),
            })
        } else {
            let in_brace;
            braced!(in_brace in input);
            let tokens: TokenStream2 = in_brace.parse()?;
            Ok(AdaptorEntityMutateInput::FnBody(quote! { { #tokens } }))
        }
    }
}

pub fn adaptor(input: AdaptorInput, relative_path_to_source_file: PathBuf) -> TokenStream2 {
    let AdaptorInput { path, config } = input;
    let repositories_input = get_repositories_input(&path, relative_path_to_source_file);

    let AdaptorConfigInput { adaptor_name, entity_prefix, client, error, adaptor_entity_inputs, .. } = config;
    let adaptor_fields: Vec<_> = repositories_input.repositories
        .iter()
        .map(|repository_input| adaptor_field(repository_input, &entity_prefix))
        .collect();

    let mut adaptor_entity_inputs: HashMap<_, _> = adaptor_entity_inputs
        .into_iter()
        .map(|adaptor_entity_input| (adaptor_entity_input.ty.clone(), adaptor_entity_input))
        .collect();

    let repositories_and_adaptor_entity_inputs: HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)> = repositories_input
        .repositories
        .iter()
        .map(|repository_input| {
            let adaptor_entity_input = adaptor_entity_inputs.remove(repository_input.ty());
            (repository_input.ty().clone(), ((*repository_input).clone(), adaptor_entity_input))
        })
        .collect();

    if adaptor_entity_inputs.len() > 0 {
        let entity_str = if adaptor_entity_inputs.len() == 1 { "entity" } else { "entities" };
        let unrecognized_entities: Vec<_> = adaptor_entity_inputs.into_keys().map(|ident| format!("{}", ident)).collect();

        panic!("unrecognized {}: {}", entity_str, unrecognized_entities.join(", "));
    }

    let namespaces: HashMap<Ident, Ident> = repositories_and_adaptor_entity_inputs
        .iter()
        .map(|(ty, value)| (ty.clone(), {
            if let Some(adaptor_entity_input) = value.1.as_ref() {
                adaptor_entity_input.namespace.as_ref().map(|namespace| namespace.clone()).unwrap_or_else(|| value.0.singular().clone())
            } else {
                value.0.singular().clone()
            }
        }))
        .collect();

    let (repository_impls, all_load_keys_by) = transpose_2(
        repositories_and_adaptor_entity_inputs
            .values()
            .map(|value| repository_impl(&adaptor_name, &entity_prefix, &namespaces, &repositories_input, &value.0, &value.1, &repositories_and_adaptor_entity_inputs))
            .collect()
    );

    let load_keys_by: Vec<_> = all_load_keys_by
        .into_iter()
        .map(|load_keys_by| load_keys_by.into_iter())
        .flatten()
        .collect::<HashMap<Ident, TokenStream2>>()
        .into_values()
        .collect();

    let tokens = quote! {
        hex_arch_paste! {
            hex_arch_lazy_static! {
                static ref RW_LOCK: std::sync::RwLock<()> = std::sync::RwLock::new(());
            }

            #[derive(Clone, Debug, Default)]
            pub struct #adaptor_name {
                #(#adaptor_fields),*
            }

            impl BaseRepository for #adaptor_name {
                type Client<'a> = #client;
                type Error = #error;

                fn read() -> std::sync::LockResult<std::sync::RwLockReadGuard<'static, ()>> { RW_LOCK.read() }
                fn write() -> std::sync::LockResult<std::sync::RwLockWriteGuard<'static, ()>> { RW_LOCK.write() }
            }

            #(#repository_impls)*

            impl #adaptor_name {
                #(#load_keys_by)*
            }
        }
    };
    tokens
}

pub fn adaptor_field(repository_input: &RepositoryInput, entity_prefix: &Ident) -> TokenStream2 {
    let ty = repository_input.ty();
    let plural = repository_input.plural();
    let key_ty = repository_input.key_ty();
    let sync_ptr = repository_input.sync_ptr();

    quote! {
        pub #plural: std::collections::HashMap<#key_ty, (#sync_ptr<#ty>, #sync_ptr<[<#entity_prefix #ty>]>)>
    }
}

pub fn repository_impl(
    adaptor_name: &Ident,
    entity_prefix: &Ident,
    namespaces: &HashMap<Ident, Ident>,
    repositories_input: &RepositoriesInput,
    repository_input: &RepositoryInput,
    adaptor_entity_input_opt: &Option<AdaptorEntityInput>,
    repositories_and_adaptor_entity_inputs: &HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)>,
) -> (TokenStream2, Vec<(Ident, TokenStream2)>) {
    let base_repository = base_repository_impl(adaptor_name, entity_prefix, repository_input);
    let (read_repository, load_keys_by) = read_repository_impl(adaptor_name, entity_prefix, namespaces, repository_input, repositories_input, adaptor_entity_input_opt.as_ref(), repositories_and_adaptor_entity_inputs);
    let write_repository = write_repository_impl(adaptor_name, entity_prefix, namespaces, repository_input, adaptor_entity_input_opt.as_ref(), repositories_and_adaptor_entity_inputs);

    (quote! { #base_repository #read_repository #write_repository }, load_keys_by)
}

pub fn base_repository_impl(adaptor_name: &Ident, entity_prefix: &Ident, repository_input: &RepositoryInput) -> TokenStream2 {
    let ty = repository_input.ty();
    let key_ty = repository_input.key_ty();

    quote! {
        hex_arch_paste! {
            impl [<#ty BaseRepository>] for #adaptor_name {
                type Key = #key_ty;
                type Record = [<#entity_prefix #ty>];
            }
        }
    }
}

pub fn read_repository_impl(
    adaptor_name: &Ident,
    entity_prefix: &Ident,
    namespaces: &HashMap<Ident, Ident>,
    repository_input: &RepositoryInput,
    repositories_input: &RepositoriesInput,
    adaptor_entity_input_opt: Option<&AdaptorEntityInput>,
    repositories_and_adaptor_entity_inputs: &HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)>,
) -> (TokenStream2, Vec<(Ident, TokenStream2)>) {
    let ty = repository_input.ty();
    let singular = repository_input.singular();
    let plural = repository_input.plural();
    let key_ty = repository_input.key_ty();
    let key_singular = repository_input.key_singular();
    let key_plural = repository_input.key_plural();
    let namespace = namespaces.get(ty).unwrap();

    let mut load_plural_body = quote! {
        hex_arch_paste! {
            Ok(load! { One, [<#entity_prefix #ty>], [<#singular _ #key_plural>], #key_ty, client, #namespace, #key_singular })
        }
    };
    let mut try_load_plural_body = quote! {
        hex_arch_paste! {
            Ok(load! { OneOrNone, [<#entity_prefix #ty>], [<#singular _ #key_plural>], #key_ty, client, #namespace, #key_singular })
        }
    };
    let mut load_all_body = quote! {
        hex_arch_paste! {
            Ok(load_all! { client, #namespace })
        }
    };

    let inward_relations = repository_input.inward_relations(repositories_input);

    let mut load_keys_by: HashMap<Ident, TokenStream2> = inward_relations
        .iter()
        .map(|(_, relation_repository)| {
            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let relation_namespace = namespaces.get(relation_ty).unwrap();
            let relation_key_singular = relation_repository.key_singular();
            let relation_key_plural = relation_repository.key_plural();

            let fn_name = op_keys_by_multiple_fn_name("load", repository_input, relation_repository);
            let fn_def = load_keys_by_multiple(repository_input, relation_repository, &quote! {
                hex_arch_paste! {
                    Ok(load_keys_by! {
                        #key_singular,
                        #key_ty,
                        [<#relation_singular _ #relation_key_plural>],
                        client,
                        #namespace,
                        #relation_namespace,
                        #relation_key_singular,
                    })
                }
            });
            (fn_name, fn_def)
        })
        .collect();

    let mut load_by_multiples: HashMap<Ident, TokenStream2> = inward_relations
        .iter()
        .map(|(relation, relation_repository)| {
            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let relation_plural = relation_repository.plural();
            let relation_key_ty = relation_repository.key_ty();
            let relation_key_singular = relation_repository.key_singular();
            let relation_key_plural = relation_repository.key_plural();
            let cardinality = &relation.cardinality;
            let relation_namespace = namespaces.get(relation_ty).unwrap();

            let relation_snake = relation.snake();

            let (order_by_field, order_by_ordering) = repositories_and_adaptor_entity_inputs
                .get(relation_ty)
                .unwrap()
                .1
                .as_ref()
                .map(|adaptor_entity_input|
                    adaptor_entity_input.children
                        .iter()
                        .find(|child_input| child_input.snake == *relation_snake)
                        .map(|child_input| (child_input.order_by_field.clone(), child_input.order_by_ordering.clone()))
                )
                .flatten()
                .unwrap_or((None, None));

            let order_by_tokens = match order_by_field {
                Some(order_by_field) => match order_by_ordering {
                    Some(order_by_ordering) => quote! { #order_by_field #order_by_ordering, },
                    None => quote! { #order_by_field asc, }
                },
                None => quote! {},
            };

            (
                op_by_multiple_fn_name("load", relation, relation_repository),
                load_by_multiple(relation, repository_input, relation_repository, &(
                    if *relation_ty == *ty {
                        quote! {
                            {
                                hex_arch_paste! {
                                    let [<#singular _ #key_plural>]: Vec<_> = #plural
                                        .into_iter()
                                        .map(|x| x.as_ref().clone())
                                        .collect();
                                    Ok(load! {
                                        #cardinality,
                                        [<#entity_prefix #ty>],
                                        [<#singular _ #key_plural>],
                                        #key_ty,
                                        client,
                                        #namespace,
                                        #key_singular,
                                    })
                                }
                            }
                        }
                    } else {
                        quote! {
                            {
                                hex_arch_paste! {
                                    let [<#relation_singular _ #relation_key_plural>]: Vec<_> = #relation_plural
                                        .into_iter()
                                        .map(|x| x.as_ref().clone())
                                        .collect();
                                    Ok(load_by! {
                                        #cardinality,
                                        [<#entity_prefix #ty>],
                                        [<#entity_prefix #relation_ty>],
                                        [<#relation_singular _ #relation_key_plural>],
                                        #relation_key_ty,
                                        client,
                                        #namespace,
                                        #relation_namespace,
                                        #relation_key_singular,
                                        #order_by_tokens
                                    })
                                }
                            }
                        }
                    }
                )),
            )
        })
        .collect();

    let mut all_load_by_multiple_keys: HashMap<Ident, TokenStream2> = inward_relations
        .iter()
        .map(|(relation, relation_repository)| {
            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let relation_key_ty = relation_repository.key_ty();
            let relation_key_singular = relation_repository.key_singular();
            let relation_key_plural = relation_repository.key_plural();
            let cardinality = &relation.cardinality;
            let relation_namespace = namespaces.get(relation_ty).unwrap();

            let relation_snake = relation.snake();

            let (order_by_field, order_by_ordering) = repositories_and_adaptor_entity_inputs
                .get(relation_ty)
                .unwrap()
                .1
                .as_ref()
                .map(|adaptor_entity_input|
                    adaptor_entity_input.children
                        .iter()
                        .find(|child_input| child_input.snake == *relation_snake)
                        .map(|child_input| (child_input.order_by_field.clone(), child_input.order_by_ordering.clone()))
                )
                .flatten()
                .unwrap_or((None, None));

            let order_by_tokens = match order_by_field {
                Some(order_by_field) => match order_by_ordering {
                    Some(order_by_ordering) => quote! { #order_by_field #order_by_ordering, },
                    None => quote! { #order_by_field asc, }
                },
                None => quote! {},
            };

            (
                op_by_multiple_keys_fn_name("load", relation, relation_repository),
                load_by_multiple_keys(relation, repository_input, relation_repository, &(
                    if *relation_ty == *ty {
                        quote! {
                            {
                                hex_arch_paste! {
                                    Ok(load! {
                                        #cardinality,
                                        [<#entity_prefix #ty>],
                                        [<#singular _ #key_plural>],
                                        #key_ty,
                                        client,
                                        #namespace,
                                        #key_singular,
                                    })
                                }
                            }
                        }
                    } else {
                        quote! {
                            {
                                hex_arch_paste! {
                                    Ok(load_by! {
                                        #cardinality,
                                        [<#entity_prefix #ty>],
                                        [<#entity_prefix #relation_ty>],
                                        [<#relation_singular _ #relation_key_plural>],
                                        #relation_key_ty,
                                        client,
                                        #namespace,
                                        #relation_namespace,
                                        #relation_key_singular,
                                        #order_by_tokens
                                    })
                                }
                            }
                        }
                    }
                )),
            )
        })
        .collect();

    for load_by in repository_input.load_bys.iter() {
        let load_by_ty = load_by.ty();
        let load_by_singular = load_by.singular();
        let load_by_plural = load_by.plural();
        let cardinality = &load_by.cardinality;

        load_by_multiples.insert(get_load_by_field_multiple_fn_name(repository_input, load_by), load_by_field_multiple(
            repository_input,
            load_by,
            &quote! {
                {
                    hex_arch_paste! {
                        Ok(load! {
                            #cardinality,
                            [<#entity_prefix #ty>],
                            #load_by_plural,
                            #load_by_ty,
                            client,
                            #namespace,
                            #load_by_singular,
                        })
                    }
                }
            }
        ));
    }

    let inward_relations_by_load_by_fn_names: HashMap<Ident, (&RelationInput, &RepositoryInput)> = inward_relations
        .iter()
        .map(|inward_relation| (op_by_multiple_fn_name("load", inward_relation.0, inward_relation.1), inward_relation.clone()))
        .collect();

    let inward_relations_by_load_by_keys_fn_names: HashMap<Ident, (&RelationInput, &RepositoryInput)> = inward_relations
        .iter()
        .map(|inward_relation| (op_by_multiple_keys_fn_name("load", inward_relation.0, inward_relation.1), inward_relation.clone()))
        .collect();

    let mut inward_relations_by_load_keys_by_fn_names: HashMap<Ident, (&RelationInput, &RepositoryInput)> = inward_relations
        .iter()
        .map(|inward_relation| (op_keys_by_multiple_fn_name("load", repository_input, inward_relation.1), inward_relation.clone()))
        .collect();

    let reflective_load_key_by_fn_name = op_keys_by_multiple_fn_name("load", repository_input, repository_input);
    let mut has_reflective_relation = false;
    for inward_relation in inward_relations.iter() {
        if *inward_relation.1.ty() == *ty {
            load_keys_by.remove(&reflective_load_key_by_fn_name);
            inward_relations_by_load_keys_by_fn_names.remove(&reflective_load_key_by_fn_name);
            has_reflective_relation = true;
            break;
        }
    };

    if let Some(adaptor_entity_input) = adaptor_entity_input_opt {
        if let Some(plural) = adaptor_entity_input.load.plural.as_ref() {
            load_plural_body = plural.clone();
        }
        if let Some(try_plural) = adaptor_entity_input.load.try_plural.as_ref() {
            try_load_plural_body = try_plural.clone();
        }
        if let Some(all) = adaptor_entity_input.load.all.as_ref() {
            load_all_body = all.clone();
        }

        let invalid_load_by_fn_name_message = format!(
            "unrecognized load.by fn for `{}`, expected one of {}",
            ty,
            load_by_multiples
                .keys()
                .map(|key| format!("`{}`", key))
                .chain(all_load_by_multiple_keys.keys().map(|key| format!("`{}`", key)))
                .collect::<Vec<_>>()
                .join(", "),
        );

        for (load_by_fn_name, body) in adaptor_entity_input.load.by.iter() {
            if load_by_multiples.contains_key(load_by_fn_name) {
                let inward_relation = inward_relations_by_load_by_fn_names.get(load_by_fn_name).unwrap();
                load_by_multiples.insert(
                    load_by_fn_name.clone(),
                    load_by_multiple(inward_relation.0, repository_input, inward_relation.1, body),
                );
            } else if all_load_by_multiple_keys.contains_key(load_by_fn_name) {
                let inward_relation = inward_relations_by_load_by_keys_fn_names.get(load_by_fn_name).unwrap();
                all_load_by_multiple_keys.insert(
                    load_by_fn_name.clone(),
                    load_by_multiple_keys(inward_relation.0, repository_input, inward_relation.1, body),
                );
            } else {
                return (syn::Error::new_spanned(load_by_fn_name, &invalid_load_by_fn_name_message).into_compile_error(), vec![]);
            }
        }
    }

    if has_reflective_relation {
        load_keys_by.insert(reflective_load_key_by_fn_name, load_keys_by_multiple(repository_input, repository_input, &quote! {
            hex_arch_paste! {
                { client; }
                Ok([<#singular _ #key_plural>])
            }
        }));
    }

    let load_by_multiples: Vec<_> = load_by_multiples.into_values().collect();
    let all_load_by_multiple_keys: Vec<_> = all_load_by_multiple_keys.into_values().collect();
    let load_keys_by: Vec<_> = load_keys_by.drain().collect();

    (
        quote! {
            hex_arch_paste! {
                impl [<#ty ReadRepository>] for #adaptor_name {

                    fn [<#plural _mut>](&mut self) -> &mut std::collections::HashMap<
                        <Self as [<#ty BaseRepository>]>::Key,
                        (
                            std::sync::Arc<#ty>,
                            std::sync::Arc<<Self as [<#ty BaseRepository>]>::Record>,
                        ),
                    >
                    {
                        &mut self.#plural
                    }

                    fn [<load_ #plural>](
                        [<#singular _ #key_plural>]: Vec<<Self as [<#ty BaseRepository>]>::Key>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error> {
                        #load_plural_body
                    }

                    fn [<try_load_ #plural>](
                        [<#singular _ #key_plural>]: Vec<<Self as [<#ty BaseRepository>]>::Key>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<Option<<Self as [<#ty BaseRepository>]>::Record>>, Self::Error> {
                        #try_load_plural_body
                    }

                    fn [<load_all_ #plural>](client: Self::Client<'_>) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error> {
                        #load_all_body
                    }

                    #(#load_by_multiples)*
                    #(#all_load_by_multiple_keys)*
                }
            }
        },
        load_keys_by,
    )
}

pub fn write_repository_impl(
    adaptor_name: &Ident,
    entity_prefix: &Ident,
    namespaces: &HashMap<Ident, Ident>,
    repository_input: &RepositoryInput,
    adaptor_entity_input_opt: Option<&AdaptorEntityInput>,
    repositories_and_adaptor_entity_inputs: &HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)>,
) -> TokenStream2 {
    if repository_input.mutability == Mutability::R {
        return quote! {};
    }

    let ty = repository_input.ty();
    let singular = repository_input.singular();
    let plural = repository_input.plural();
    let namespace = namespaces.get(ty).unwrap();
    let key_singular = repository_input.key_singular();
    let key_plural = repository_input.key_plural();

    let mut insert_body = quote! {
        hex_arch_paste! {
            if [<#singular _posts>].len() == 0 {
                return Ok(vec![])
            }

            let [<adaptor_ #singular _posts>]: Vec<[<#entity_prefix #ty Post>]> = [<#singular _posts>].into_iter().map(|post| post.into()).collect();
            Ok(insert! { #ty, #namespace, [<adaptor_ #singular _posts>], client })
        }
    };
    let mut update_body = quote! {
        hex_arch_paste! {
            if [<#singular _patches>].len() == 0 {
                return Ok(vec![])
            }

            let [<adaptor_ #singular _patches>]: Vec<[<#entity_prefix #ty Patch>]> = [<#singular _patches>].into_iter().map(|patch| patch.into()).collect();
            Ok(update! { #ty, #namespace, [<adaptor_ #singular _patches>], client })
        }
    };
    let mut delete_body = quote! {
        hex_arch_paste! {
            if [<#singular _ #key_plural>].len() == 0 {
                return Ok(0)
            }

            let [<deleted_ #plural>] = delete! { #ty, #namespace, #key_singular, client, [<#singular _ #key_plural>] };
            Ok([<deleted_ #plural>].len())
        }
    };

    if let Some(adaptor_entity_input) = adaptor_entity_input_opt {
        if let Some(insert) = adaptor_entity_input.insert.as_ref() {
            insert_body = match &insert {
                AdaptorEntityMutateInput::FnBody(tokens) => tokens.clone(),
                AdaptorEntityMutateInput::Full => {
                    let children = &adaptor_entity_input.children;
                    let reverse_linked_children = &adaptor_entity_input.reverse_linked_children;

                    let base = format_ident!("{}_{}", format!("{}", entity_prefix).to_case(Case::Snake), singular);
                    let full_ty = format_ident!("Full{}Post", ty);

                    let ChildrenUtils { child_snakes, child_singulars, child_plurals, is_child_vec_types, child_tys, .. } = children_utils(children, repositories_and_adaptor_entity_inputs, "Post");
                    let ChildrenUtils {
                        child_snakes: reverse_linked_child_snakes,
                        child_singulars: reverse_linked_child_singulars,
                        child_plurals: reverse_linked_child_plurals,
                        child_key_singulars: reverse_linked_child_key_singulars,
                        child_key_plurals: reverse_linked_child_key_plurals,
                        is_child_vec_types: reverse_linked_is_child_vec_types,
                        inner_tys: reverse_linked_inner_tys,
                        child_tys: reverse_linked_child_tys,
                    } = children_utils(reverse_linked_children, repositories_and_adaptor_entity_inputs, "Post");
                    let reverse_linked_key_singulars: Vec<_> = reverse_linked_inner_tys
                        .into_iter()
                        .map(|inner_ty|
                            repositories_and_adaptor_entity_inputs
                                .get(&inner_ty)
                                .unwrap()
                                .0
                                .key_singular()
                        )
                        .collect();

                    let flattened_child_posts: Vec<_> = izip!(child_singulars.iter(), is_child_vec_types.into_iter())
                        .map(|(child_singular, is_child_vec_type)| {
                            if is_child_vec_type {
                                quote! {
                                    let [<#child_singular _posts>]: Vec<_> = hex_arch_izip!([<adaptor_ #plural>].iter(), [<all_ #child_singular _posts>].into_iter())
                                        .map(|(#singular, [<#child_singular _posts>])| {
                                            let [<#singular _ #key_singular>] = #singular.#key_singular;
                                            [<#child_singular _posts>]
                                                .into_iter()
                                                .map(move |mut [<#child_singular _post>]| {
                                                    [<#child_singular _post>].[<set_ #singular _ #key_singular>]([<#singular _ #key_singular>]);
                                                    [<#child_singular _post>]
                                                })
                                        })
                                        .flatten()
                                        .collect();
                                }
                            } else {
                                quote! {
                                    let [<#child_singular _posts>]: Vec<_> = hex_arch_izip!([<adaptor_ #plural>].iter(), [<all_ #child_singular _posts>].into_iter())
                                        .map(|(#singular, mut [<#child_singular _post>])| {
                                            [<#child_singular _post>].[<set_ #singular _ #key_singular>](#singular.#key_singular);
                                            [<#child_singular _post>]
                                        })
                                        .collect();
                                }
                            }
                        })
                        .collect();

                    let flattened_reverse_linked_child_posts_before_parent_inserts: Vec<_> = izip!(
                        reverse_linked_child_singulars.iter(),
                        reverse_linked_child_plurals.iter(),
                        reverse_linked_is_child_vec_types.iter(),
                    )
                        .map(|(child_singular, child_plural, is_child_vec_type)| {
                            if *is_child_vec_type {
                                quote! {
                                    hex_arch_paste! {
                                        let [<#child_singular chunk_lengths>]: Vec<_> = [<all_ #child_singular _posts>].iter().map(|[<#child_singular _posts>]| [<#child_singular _posts>].len()).collect();
                                        let [<#child_singular _posts>] = [<all_ #child_singular _posts>]
                                                .into_iter()
                                                .map(|[<#child_singular _posts>]| [<#child_singular _posts>].into_iter())
                                                .flatten()
                                                .collect();
                                        let mut [<all_ #child_plural>] = Self::[<insert_ #child_plural>]([<#child_singular _posts>], client)?.into_iter();
                                        let #child_plural: Vec<Vec<_>> = [<#child_singular chunk_lengths>]
                                            .into_iter()
                                            .map(|chunk_length| [<all_ #child_plural>].take(chunk_length).collect())
                                            .collect();
                                    }
                                }
                            } else {
                                quote! {
                                    hex_arch_paste! {
                                        let [<#child_singular _posts>] = [<all_ #child_singular _posts>];
                                        let #child_plural = Self::[<insert_ #child_plural>]([<#child_singular _posts>], client)?;
                                    }
                                }
                            }
                        })
                        .collect();

                    let flattened_reverse_linked_child_posts_after_parent_inserts: Vec<_> = izip!(
                        reverse_linked_child_singulars.iter(),
                        reverse_linked_child_key_singulars.iter(),
                        reverse_linked_child_key_plurals.iter(),
                        reverse_linked_is_child_vec_types.iter(),
                        reverse_linked_child_snakes.iter(),
                        reverse_linked_key_singulars.iter(),
                    )
                        .map(|(child_singular, child_key_singular, child_key_plural, is_child_vec_type, child_snake, key_singular)| {
                            if *is_child_vec_type {
                                quote! {
                                    hex_arch_paste! {
                                        [<adaptor_ #singular _post>].[<set_ #child_singular _ #child_key_plural>](#child_snake.into_iter().map(|#child_singular| #child_singular.#key_singular).collect());
                                    }
                                }
                            } else {
                                quote! {
                                    hex_arch_paste! {
                                        [<adaptor_ #singular _post>].[<set_ #child_singular _ #child_key_singular>](#child_snake.#key_singular);
                                    }
                                }
                            }
                        })
                        .collect();

                    quote! {
                        if [<#singular _posts>].len() == 0 {
                            return Ok(vec![])
                        }

                        let [<full_ #singular _posts>]: Vec<#full_ty> = [<#singular _posts>].into_iter().map(|post| post.into()).collect();

                        let (mut [<adaptor_ #singular _posts>], all_child_posts) = transpose(
                            [<full_ #singular _posts>]
                                .into_iter()
                                .map(|full_post| ( full_post.#base, ( #(full_post.#child_snakes ,)* #(full_post.#reverse_linked_child_snakes ,)* ) ))
                                .collect()
                        );

                        #( let mut [<all_ #child_singulars _posts>]: Vec<#child_tys> = Vec::with_capacity(all_child_posts.len()); )*
                        #( let mut [<all_ #reverse_linked_child_singulars _posts>]: Vec<#reverse_linked_child_tys> = Vec::with_capacity(all_child_posts.len()); )*

                        for ( #([<#child_singulars _post>] ,)* #([<#reverse_linked_child_singulars _post>] ,)* ) in all_child_posts.into_iter() {
                            #( [<all_ #child_singulars _posts>].push([<#child_singulars _post>]); )*
                            #( [<all_ #reverse_linked_child_singulars _posts>].push([<#reverse_linked_child_singulars _post>]); )*
                        }

                        #( #flattened_reverse_linked_child_posts_before_parent_inserts )*

                        for ([<adaptor_ #singular _post>] #(, #reverse_linked_child_snakes )*) in hex_arch_izip!([<adaptor_ #singular _posts>].iter_mut() #(, #reverse_linked_child_plurals.into_iter() )*) {
                            #( #flattened_reverse_linked_child_posts_after_parent_inserts )*
                        }

                        let [<adaptor_ #plural>] = insert! { #ty, #namespace, [<adaptor_ #singular _posts>], client };

                        #( #flattened_child_posts )*

                        #( Self::[<insert_ #child_plurals>]([<#child_singulars _posts>], client)?; )*

                        Ok([<adaptor_ #plural>])
                    }
                },
            };
        }
        if let Some(update) = adaptor_entity_input.update.as_ref() {
            update_body = match &update {
                AdaptorEntityMutateInput::FnBody(tokens) => tokens.clone(),
                AdaptorEntityMutateInput::Full => {
                    let children = &adaptor_entity_input.children;
                    let base = format_ident!("{}_{}", format!("{}", entity_prefix).to_case(Case::Snake), singular);
                    let full_ty = format_ident!("Full{}Patch", ty);
                    let (child_snakes, is_child_vec_types, child_tys) = transpose_3(
                        children.iter().map(|child| {
                            let is_child_vec_type = is_vec_type(&child.ty);
                            let child_ty = if is_child_vec_type { as_post(&child.ty) } else { format!("Option<{}>", as_patch(&child.ty)).parse().unwrap() };
                            (
                                child.snake.clone(),
                                is_child_vec_type,
                                child_ty,
                            )
                        }).collect()
                    );

                    let repository_relations: HashMap<Ident, RelationInput> = repository_input.relations
                        .clone()
                        .into_iter()
                        .map(|relation| (relation.snake().clone(), relation))
                        .collect();

                    let (child_singulars, child_plurals, child_snake_names, child_inner_tys, child_cardinalities) = transpose_5(
                        children
                            .iter()
                            .map(|child| {
                                let repository_and_adaptor_entity_input = repositories_and_adaptor_entity_inputs.get(&inner_ty(&child.ty)).unwrap();
                                (
                                    repository_and_adaptor_entity_input.0.singular(),
                                    repository_and_adaptor_entity_input.0.plural(),
                                    &child.snake,
                                    inner_ty(&child.ty),
                                    &repository_relations
                                        .get(&child.snake)
                                        .unwrap()
                                        .cardinality,
                                )
                            })
                            .collect(),
                    );

                    let flattened_child_payloads: Vec<_> = izip!(child_singulars.iter(), is_child_vec_types.iter())
                        .map(|(child_singular, is_child_vec_type)| {
                            if *is_child_vec_type {
                                quote! {
                                    let [<#child_singular _payloads>]: Vec<_> = hex_arch_izip!([<adaptor_ #singular _patches>].iter(), [<all_ #child_singular _payloads>].into_iter())
                                        .filter_map(|([<adaptor_ #singular _patch>], [<#child_singular _posts>])|
                                            [<#child_singular _posts>].map(|[<#child_singular _posts>]| {
                                                let [<#singular _ #key_singular>] = [<adaptor_ #singular _patch>].#key_singular;
                                                [<#child_singular _posts>]
                                                    .into_iter()
                                                    .map(move |mut [<#child_singular _post>]| {
                                                        [<#child_singular _post>].[<set_ #singular _ #key_singular>]([<#singular _ #key_singular>]);
                                                        [<#child_singular _post>]
                                                    })
                                            })
                                        )
                                        .flatten()
                                        .collect();
                                }
                            } else {
                                quote! {
                                    let [<#child_singular _payloads>] = [<all_ #child_singular _payloads>]
                                        .into_iter()
                                        .filter_map(|x| x)
                                        .collect();
                                }
                            }
                        })
                        .collect();

                    let child_ops: Vec<_> = izip!(
                        child_singulars.iter(),
                        child_plurals.iter(),
                        child_snake_names.iter(),
                        child_inner_tys.iter(),
                        child_cardinalities.iter(),
                        is_child_vec_types.iter(),
                    )
                        .map(|(child_singular, child_plural, child_snake_name, child_inner_ty, child_cardinality, is_child_vec_type)| {
                            if *is_child_vec_type {
                                let keys_from_children = match child_cardinality {
                                    Cardinality::Many|Cardinality::AtLeastOne => quote! {
                                        hex_arch_paste! {
                                            let #child_snake_name: Vec<<Self as [<#child_inner_ty BaseRepository>]>::Key> = #child_snake_name
                                                .into_iter()
                                                .map(|x| x.0.into())
                                                .collect();
                                        }
                                    },
                                    _ => quote! {},
                                };
                                quote! {
                                    hex_arch_paste! {
                                        if [<#child_singular _payloads>].len() > 0 {
                                            let #child_snake_name = Self::[<load_ #child_snake_name _by_ #singular _ #key_plural>](
                                                [<adaptor_ #singular _patches>]
                                                    .iter()
                                                    .map(|[<adaptor_ #singular _patch>]| [<adaptor_ #singular _patch>].#key_singular)
                                                    .collect(),
                                                client,
                                            )?;
                                            #keys_from_children
                                            Self::[<delete_ #child_plural>](#child_snake_name, client)?;
                                            Self::[<insert_ #child_plural>]([<#child_singular _payloads>], client)?;
                                        }
                                    }
                                }
                            } else {
                                quote! {
                                    hex_arch_paste! {
                                        Self::[<update_ #child_plural>]([<#child_singular _payloads>], client)?;
                                    }
                                }
                            }
                        })
                        .collect();

                    quote! {
                        if [<#singular _patches>].len() == 0 {
                            return Ok(vec![])
                        }

                        let [<full_ #singular _patches>]: Vec<#full_ty> = [<#singular _patches>].into_iter().map(|patch| patch.into()).collect();

                        let ([<adaptor_ #singular _patches>], all_child_payloads) = transpose(
                            [<full_ #singular _patches>]
                                .into_iter()
                                .map(|full_post| ( full_post.#base, ( #(full_post.#child_snakes),* ) ))
                                .collect()
                        );

                        #(
                            let mut [<all_ #child_singulars _payloads>]: Vec<Option<#child_tys>> = Vec::with_capacity(all_child_payloads.len());
                        )*
                        for (#([<#child_singulars _payload>]),*) in all_child_payloads.into_iter() {
                            #(
                                [<all_ #child_singulars _payloads>].push([<#child_singulars _payload>]);
                            )*
                        }

                        #( #flattened_child_payloads )*

                        #( #child_ops )*

                        let [<adaptor_ #plural>] = update! { #ty, #namespace, [<adaptor_ #singular _patches>], client };

                        Ok([<adaptor_ #plural>])
                    }
                },
            };
        }
        if let Some(delete) = adaptor_entity_input.delete.as_ref() {
            delete_body = match &delete {
                AdaptorEntityMutateInput::FnBody(tokens) => tokens.clone(),
                AdaptorEntityMutateInput::Full => panic!("full delete is implemented by default for all children specified, `delete` should be only specified for custom delete operations"),
            };
        } else {
            delete_body = get_delete_body(ty, namespaces, repositories_and_adaptor_entity_inputs);
        }
    }

    quote! {
        hex_arch_paste! {
            impl [<#ty WriteRepository>] for #adaptor_name {
                fn [<insert_ #plural>](
                    [<#singular _posts>]: Vec<[<#ty Post>]>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error> {
                    #insert_body
                }

                fn [<update_ #plural>](
                    [<#singular _patches>]: Vec<[<#ty Patch>]>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error> {
                    #update_body
                }

                fn [<delete_ #plural>](
                    [<#singular _ #key_plural>]: Vec<<Self as [<#ty BaseRepository>]>::Key>,
                    client: Self::Client<'_>,
                ) -> Result<usize, Self::Error> {
                    #delete_body
                }
            }
        }
    }
}

fn get_delete_body(
    ty: &Ident,
    namespaces: &HashMap<Ident, Ident>,
    repositories_and_adaptor_entity_inputs: &HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)>,
) -> TokenStream2 {
    let repository_and_adaptor_entity_input = repositories_and_adaptor_entity_inputs.get(ty).unwrap();
    let repository_input = &repository_and_adaptor_entity_input.0;
    let adaptor_entity_input_opt = &repository_and_adaptor_entity_input.1;

    let singular = repository_input.singular();
    let plural = repository_input.plural();
    let namespace = namespaces.get(ty).unwrap();
    let key_singular = repository_input.key_singular();
    let key_plural = repository_input.key_plural();

    let (child_singulars, child_plurals, child_key_plurals, reverse_linked_child_singulars, reverse_linked_child_plurals, reverse_linked_child_key_plurals) = match adaptor_entity_input_opt.as_ref() {
        None => (vec![], vec![], vec![], vec![], vec![], vec![]),
        Some(adaptor_entity_input) => {
            let ChildrenUtils { child_singulars, child_plurals, child_key_plurals, .. } = children_utils(&adaptor_entity_input.children, repositories_and_adaptor_entity_inputs, "placeholder");
            let ChildrenUtils {
                child_singulars: reverse_linked_child_singulars,
                child_plurals: reverse_linked_child_plurals,
                child_key_plurals: reverse_linked_child_key_plurals,
                ..
            } = children_utils(&adaptor_entity_input.reverse_linked_children, repositories_and_adaptor_entity_inputs, "placeholder");
            (child_singulars, child_plurals, child_key_plurals, reverse_linked_child_singulars, reverse_linked_child_plurals, reverse_linked_child_key_plurals)
        },
    };

    quote! {
        hex_arch_paste! {
            if [<#singular _ #key_plural>].len() == 0 {
                return Ok(0)
            }

            #(
                let [<num_deleted_ #child_plurals>] = Self::[<delete_ #child_plurals>](
                    Self::[<load_ #child_singulars _ #child_key_plurals _by_ #singular _ #key_plural>]([<#singular _ #key_plural>].clone(), client)?,
                    client,
                )?;
            )*

            let [<deleted_ #plural>] = delete! { #ty, #namespace, #key_singular, client, [<#singular _ #key_plural>].clone() };

            #(
                let [<num_deleted_ #reverse_linked_child_plurals>] = Self::[<delete_ #reverse_linked_child_plurals>](
                    Self::[<load_ #reverse_linked_child_singulars _ #reverse_linked_child_key_plurals _by_ #singular _ #key_plural>]([<#singular _ #key_plural>].clone(), client)?,
                    client,
                )?;
            )*

            Ok( [<deleted_ #plural>].len() #(+ [<num_deleted_ #child_plurals>])* #(+ [<num_deleted_ #reverse_linked_child_plurals>])* )
        }
    }
}

fn is_vec_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<_> = type_path.path.segments.clone().into_iter().collect();
            (segments.len() == 1 && segments[0].ident == "Vec") ||
            (segments.len() == 3 && segments[1].ident == "std" && segments[1].ident == "vec" && segments[1].ident == "Vec")
        },
        _ => false,
    }
}

fn inner_ty(ty: &Type) -> Ident {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<_> = type_path.path.segments.clone().into_iter().collect();
            match &segments[segments.len() - 1].arguments {
                syn::PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => match angle_bracketed_generic_arguments.args.first().unwrap() {
                    syn::GenericArgument::Type(inner_ty) => format_ident!("{}", format!("{}", quote! { #inner_ty })),
                    _ => unreachable!(),
                },
                syn::PathArguments::None => format_ident!("{}", format!("{}", quote! { #ty })),
                _ => unreachable!(),
            }
        },
        _ => unreachable!(),
    }
}

fn append_to_inner_ty(ty: &Type, tail: Ident) -> TokenStream2 {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<_> = type_path.path.segments.clone().into_iter().collect();
            match &segments[segments.len() - 1].arguments {
                syn::PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => match angle_bracketed_generic_arguments.args.first().unwrap() {
                    syn::GenericArgument::Type(inner_ty) => format!("{}<{}{}>", segments[segments.len() - 1].ident, format!("{}", quote! { #inner_ty }), tail).parse().unwrap(),
                    _ => unreachable!(),
                },
                syn::PathArguments::None => format!("{}{}", segments[segments.len() - 1].ident, tail).parse().unwrap(),
                _ => unreachable!(),
            }
        },
        _ => unreachable!(),
    }
}

fn as_post(ty: &Type) -> TokenStream2 {
    append_to_inner_ty(ty, format_ident!("Post"))
}

fn as_patch(ty: &Type) -> TokenStream2 {
    append_to_inner_ty(ty, format_ident!("Patch"))
}

struct ChildrenUtils<'a> {
    child_snakes: Vec<&'a Ident>,
    child_singulars: Vec<&'a Ident>,
    child_plurals: Vec<&'a Ident>,
    child_key_singulars: Vec<&'a Ident>,
    child_key_plurals: Vec<&'a Ident>,
    is_child_vec_types: Vec<bool>,
    inner_tys: Vec<Ident>,
    child_tys: Vec<TokenStream2>,
}

fn children_utils<'a, 'b: 'a, 'c: 'a>(
    children: &'b Vec<ChildInput>,
    repositories_and_adaptor_entity_inputs: &'c HashMap<Ident, (RepositoryInput, Option<AdaptorEntityInput>)>,
    tail: &str,
) -> ChildrenUtils<'a> {
    let (
        child_snakes,
        child_singulars,
        child_plurals,
        child_key_singulars,
        child_key_plurals,
        is_child_vec_types,
        inner_tys,
        child_tys,
    ) = transpose_8(
        children.iter().map(|child| {
            let repository_and_adaptor_entity_input = repositories_and_adaptor_entity_inputs.get(&inner_ty(&child.ty)).unwrap();
            (
                &child.snake,
                repository_and_adaptor_entity_input.0.singular(),
                repository_and_adaptor_entity_input.0.plural(),
                repository_and_adaptor_entity_input.0.key_singular(),
                repository_and_adaptor_entity_input.0.key_plural(),
                is_vec_type(&child.ty),
                inner_ty(&child.ty),
                append_to_inner_ty(&child.ty, format_ident!("{}", tail)),
            )
        }).collect()
    );
    ChildrenUtils { child_snakes, child_singulars, child_plurals, child_key_singulars, child_key_plurals, is_child_vec_types, inner_tys, child_tys }
}
