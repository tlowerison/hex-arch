extern crate itertools;
extern crate proc_macro;
#[macro_use] extern crate quote;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::collections::HashMap;
use syn::{braced, Field, Ident, parenthesized, parse_macro_input, token, Token, Type};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

#[derive(Clone, Debug)]
enum Cardinality {
    One,
    OneOrNone,
    AtLeastOne,
    Many,
}

impl From<&Ident> for Cardinality {
    fn from(ident: &Ident) -> Cardinality {
        match &*format!("{}", ident) {
            "One" => Cardinality::One,
            "OneOrNone" => Cardinality::OneOrNone,
            "AtLeastOne" => Cardinality::AtLeastOne,
            "Many" => Cardinality::Many,
            other => panic!("unrecognized relation cardinality: expected one of {{One, OneOrNone, Many, AtLeastOne}}, received {}", other),
        }
    }
}

#[derive(Clone, Debug)]
enum Mutability {
    R,
    RW,
}

impl From<&Ident> for Mutability {
    fn from(ident: &Ident) -> Mutability {
        let string = format!("{}", ident);
        match &*string {
            "R" => Mutability::R,
            "RW" => Mutability::RW,
            other => panic!("unrecognized mutability: expected one of {{R, RW}}, received {}", other),
        }
    }
}

#[derive(Clone, Debug)]
enum Syncability {
    Sync,
    Unsync,
}

impl From<&Ident> for Syncability {
    fn from(ident: &Ident) -> Syncability {
        let string = format!("{}", ident);
        match &*string {
            "sync" => Syncability::Sync,
            "unsync" => Syncability::Unsync,
            other => panic!("unrecognized syncability: expected one of {{sync, unsync}}, received {}", other),
        }
    }
}

impl Syncability {
    fn entity_value_pointer(&self) -> TokenStream2 {
        match &self {
            Syncability::Sync => "std::sync::Arc".parse().unwrap(),
            Syncability::Unsync => "std::rc::Rc".parse().unwrap(),
        }
    }
}

struct LoadOps {
    in_single: TokenStream2,
    in_multiple: TokenStream2,
}

struct LoadOpsConfig<'a> {
    name: &'a Ident,
    snake_name: &'a Ident,
    relation_name: &'a Ident,
    relation_snake_name: &'a Ident,
    relation_singular_snake_name: &'a Ident,
    relation_cardinality: &'a Cardinality,
    entity_value_pointer: &'a TokenStream2,
}

fn get_load_ops(config: LoadOpsConfig<'_>) -> LoadOps {
    let LoadOpsConfig { name, snake_name, relation_name, relation_snake_name, relation_singular_snake_name, relation_cardinality, entity_value_pointer } = config;

    let get_by_single_id_fn_name = get_op_by_single_id_fn_name("get", relation_cardinality, relation_singular_snake_name, snake_name);
    let get_by_multiple_ids_fn_name = get_op_by_multiple_ids_fn_name("get", relation_cardinality, relation_singular_snake_name, snake_name);

    match relation_cardinality {
        Cardinality::One => LoadOps {
            in_single: quote! {
                let related_entity = self.#get_by_single_id_fn_name([<#snake_name _id>].clone(), load_relations, client)?;
                loaded_relations.#relation_snake_name = Some(Box::new(related_entity));
            },
            in_multiple: quote! {
                let related_entities = self.#get_by_multiple_ids_fn_name([<#snake_name _ids>].clone(), load_relations, client)?;

                for (loaded_relations, related_entity) in repositories_izip!(all_loaded_relations.iter_mut(), related_entities.into_iter()) {
                    loaded_relations.#relation_snake_name = Some(Box::new(related_entity));
                }
            },
        },
        Cardinality::OneOrNone => LoadOps {
            in_single: quote! {
                let related_entity_option = self.#get_by_single_id_fn_name([<#snake_name _id>].clone(), load_relations, client)?;
                loaded_relations.#relation_snake_name = Some(Box::new(related_entity_option));
            },
            in_multiple: quote! {
                let related_entity_options = self.#get_by_multiple_ids_fn_name([<#snake_name _ids>].clone(), load_relations, client)?;

                for (loaded_relations, related_entity_option) in repositories_izip!(all_loaded_relations.iter_mut(), related_entity_options.into_iter()) {
                    loaded_relations.#relation_snake_name = Some(Box::new(related_entity_option));
                }
            },
        },
        Cardinality::Many => LoadOps {
            in_single: quote! {
                let related_entities_and_ids = self.#get_by_single_id_fn_name([<#snake_name _id>].clone(), load_relations, client)?;
                let related_entities: Vec<_> = related_entities_and_ids.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                loaded_relations.#relation_snake_name = Some(Box::new(related_entities));
            },
            in_multiple: quote! {
                let all_related_entities_and_ids = self.#get_by_multiple_ids_fn_name([<#snake_name _ids>].clone(), load_relations, client)?;

                let mut all_related_entities_by_parent_ids: std::collections::HashMap<
                    <Self as [<#name BaseRepository>]>::Id,
                    std::collections::HashMap<
                        <Self as [<#relation_name BaseRepository>]>::Id,
                        Vec<Entity<
                            #entity_value_pointer<#relation_name>,
                            [<Loaded #relation_name Relations>],
                        >>,
                    >,
                > = std::collections::HashMap::default();

                for (related_entity, related_id, parent_id) in all_related_entities_and_ids.into_iter() {
                    if !all_related_entities_by_parent_ids.contains_key(&parent_id) {
                        all_related_entities_by_parent_ids.insert(parent_id.clone(), std::collections::HashMap::default());
                    }

                    let all_related_entities_by_related_ids = all_related_entities_by_parent_ids.get_mut(&parent_id).unwrap();

                    if !all_related_entities_by_related_ids.contains_key(&related_id) {
                        all_related_entities_by_related_ids.insert(related_id, vec![related_entity]);
                    } else {
                        all_related_entities_by_related_ids.get_mut(&related_id).unwrap().push(related_entity);
                    }
                }

                for (i, loaded_relations) in all_loaded_relations.iter_mut().enumerate() {
                    loaded_relations.#relation_snake_name = Some(Box::new(match all_related_entities_by_parent_ids.get_mut(&[<#snake_name _ids>][i]) {
                        Some(all_related_entities_by_related_ids) => {
                            all_related_entities_by_related_ids
                                .values_mut()
                                .map(|related_entity_dupes| related_entity_dupes.pop().unwrap())
                                .collect()
                        },
                        None => vec![],
                    }));
                }
            },
        },
        Cardinality::AtLeastOne => LoadOps {
            in_single: quote! {
                let related_entities_and_ids = self.#get_by_single_id_fn_name([<#snake_name _id>].clone(), load_relations, client)?;
                let related_entities: Vec<_> = related_entities_and_ids.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                if related_entities.len() == 0 {
                    return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                }
                loaded_relations.#relation_snake_name = Some(Box::new(related_entities));
            },
            in_multiple: quote! {
                let all_related_entities_and_ids = self.#get_by_multiple_ids_fn_name([<#snake_name _ids>].clone(), load_relations, client)?;

                let mut all_related_entities_by_parent_ids: std::collections::HashMap<
                    <Self as [<#name BaseRepository>]>::Id,
                    std::collections::HashMap<
                        <Self as [<#relation_name BaseRepository>]>::Id,
                        Vec<Entity<
                            #entity_value_pointer<#relation_name>,
                            [<Loaded #relation_name Relations>],
                        >>,
                    >,
                > = std::collections::HashMap::default();

                for (related_entity, related_id, parent_id) in all_related_entities_and_ids.into_iter() {
                    if !all_related_entities_by_parent_ids.contains_key(&parent_id) {
                        all_related_entities_by_parent_ids.insert(parent_id.clone(), std::collections::HashMap::default());
                    }

                    let all_related_entities_by_related_ids = all_related_entities_by_parent_ids.get_mut(&parent_id).unwrap();

                    if !all_related_entities_by_related_ids.contains_key(&related_id) {
                        all_related_entities_by_related_ids.insert(related_id, vec![related_entity]);
                    } else {
                        all_related_entities_by_related_ids.get_mut(&related_id).unwrap().push(related_entity);
                    }
                }

                for (i, loaded_relations) in all_loaded_relations.iter_mut().enumerate() {
                    loaded_relations.#relation_snake_name = Some(Box::new(match all_related_entities_by_parent_ids.get_mut(&[<#snake_name _ids>][i]) {
                        Some(all_related_entities_by_related_ids) => {
                            let related_entities: Vec<_> = all_related_entities_by_related_ids
                                .values_mut()
                                .map(|related_entity_dupes| related_entity_dupes.pop().unwrap())
                                .collect();

                            if related_entities.len() == 0 {
                                return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                            }

                            related_entities
                        },
                        None => return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found()),
                    }));
                }
            },
        },
    }
}

struct LoadOpsBy {
    load_by_multiple_ids_fn_def: TokenStream2,
    load_by_multiple_ids_fn_def_with_todo: TokenStream2,
    get_by_single_id_fn_def: TokenStream2,
    get_by_multiple_ids_fn_def: TokenStream2,
}

struct LoadOpsByConfig<'a> {
    name: &'a Ident,
    snake_name: &'a Ident,
    relation_name: &'a Ident,
    relation_singular_snake_name: &'a Ident,
    relation_cardinality: &'a Cardinality,
    all_relation_snake_names: &'a Vec<Ident>,
    all_load_ops_in_single: &'a Vec<TokenStream2>,
    all_load_ops_in_multiple: &'a Vec<TokenStream2>,
    entity_value_pointer: &'a TokenStream2,
}

fn get_load_ops_by(config: LoadOpsByConfig<'_>) -> LoadOpsBy {
    let LoadOpsByConfig {
        name,
        snake_name,
        relation_name,
        relation_singular_snake_name,
        relation_cardinality,
        all_relation_snake_names,
        all_load_ops_in_single,
        all_load_ops_in_multiple,
        entity_value_pointer,
    } = config;

    let load_by_multiple_ids_fn_name = get_op_by_multiple_ids_fn_name("load", relation_cardinality, snake_name, relation_singular_snake_name);
    let get_by_single_id_fn_name = get_op_by_single_id_fn_name("get", relation_cardinality, snake_name, relation_singular_snake_name);
    let get_by_multiple_ids_fn_name = get_op_by_multiple_ids_fn_name("get", relation_cardinality, snake_name, relation_singular_snake_name);

    match relation_cardinality {
        Cardinality::One => LoadOpsBy {
            load_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<<Self as [<#name BaseRepository>]>::Record>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                ;
            } },
            load_by_multiple_ids_fn_def_with_todo: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<<Self as [<#name BaseRepository>]>::Record>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                {
                    todo!()
                }
            } },
            get_by_single_id_fn_def: quote! { repositories_paste! {
                fn #get_by_single_id_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _id>]: <Self as [<#relation_name BaseRepository>]>::Id,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Entity<
                        #entity_value_pointer<#name>,
                        [<Loaded #name Relations>],
                    >,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_record = Self::#load_by_multiple_ids_fn_name(vec![[<#relation_singular_snake_name _id>]], client)?.pop().unwrap();

                    let rc_record = self.[<store_ #snake_name s>](vec![&adaptor_record]).pop().unwrap();

                    let mut loaded_relations = [<Loaded #name Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _id>] = adaptor_record.as_ref().clone();
                            #all_load_ops_in_single
                        }
                    )*

                    Ok(Entity {
                        value: rc_record,
                        relations: loaded_relations,
                    })
                }
            } },
            get_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #get_by_multiple_ids_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<Entity<
                        #entity_value_pointer<#name>,
                        [<Loaded #name Relations>],
                    >>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let num_requested_records = [<#relation_singular_snake_name _ids>].len();
                    let adaptor_records = Self::#load_by_multiple_ids_fn_name([<#relation_singular_snake_name _ids>], client)?;

                    if adaptor_records.len() != num_requested_records {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());

                    let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<<Self as [<#name BaseRepository>]>::Id> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                            #all_load_ops_in_multiple
                        }
                    )*

                    Ok(
                        repositories_izip!(rc_records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(rc_record, loaded_relations)| Entity {
                                value: rc_record,
                                relations: loaded_relations,
                            })
                            .collect()
                    )
                }
            } },
        },
        Cardinality::OneOrNone => LoadOpsBy {
            load_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<Option<<Self as [<#name BaseRepository>]>::Record>>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                ;
            } },
            load_by_multiple_ids_fn_def_with_todo: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<Option<<Self as [<#name BaseRepository>]>::Record>>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                {
                    todo!()
                }
            } },
            get_by_single_id_fn_def: quote! { repositories_paste! {
                fn #get_by_single_id_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _id>]: <Self as [<#relation_name BaseRepository>]>::Id,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Option<Entity<
                        #entity_value_pointer<#name>,
                        [<Loaded #name Relations>]
                    >>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_record_option = Self::#load_by_multiple_ids_fn_name(vec![[<#relation_singular_snake_name _id>]], client)?.pop().unwrap();

                    let adaptor_record = match adaptor_record_option {
                        Some(adaptor_record) => adaptor_record,
                        None => return Ok(None),
                    };

                    let rc_record = self.[<store_ #snake_name s>](vec![&adaptor_record]).pop().unwrap();

                    let mut loaded_relations = [<Loaded #name Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _id>] = adaptor_record.as_ref().clone();
                            #all_load_ops_in_single
                        }
                    )*

                    Ok(Some(Entity {
                        value: rc_record,
                        relations: loaded_relations,
                    }))
                }

            } },
            get_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #get_by_multiple_ids_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<Option<Entity<
                        #entity_value_pointer<#name>,
                        [<Loaded #name Relations>]
                    >>>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let num_requested_records = [<#relation_singular_snake_name _ids>].len();
                    let adaptor_record_options = Self::#load_by_multiple_ids_fn_name([<#relation_singular_snake_name _ids>], client)?;

                    if adaptor_record_options.len() != num_requested_records {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (adaptor_records, indices_of_existing_records) = repositories_transpose_2::<<Self as [<#name BaseRepository>]>::Record, usize>(
                        adaptor_record_options
                            .into_iter()
                            .enumerate()
                            .filter_map(|(i, adaptor_record_option)| adaptor_record_option.map(|adaptor_record| (adaptor_record, i)))
                            .collect()
                    );

                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());

                    let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                            #all_load_ops_in_multiple
                        }
                    )*

                    let mut entity_options: Vec<Option<Entity<
                        #entity_value_pointer<#name>,
                        [<Loaded #name Relations>],
                    >>> = Vec::with_capacity(num_requested_records);

                    let mut option_index = 0;
                    for (i, (rc_record, loaded_relations)) in repositories_izip!(rc_records.into_iter(), all_loaded_relations.into_iter()).enumerate() {
                        let desired_option_index = indices_of_existing_records[i];
                        while option_index < desired_option_index {
                            entity_options.push(None);
                            option_index += 1;
                        }
                        entity_options.push(Some(Entity {
                            value: rc_record,
                            relations: loaded_relations,
                        }));
                    }

                    Ok(entity_options)
                }
            } },
        },
        Cardinality::Many => LoadOpsBy {
            load_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<(<Self as [<#name BaseRepository>]>::Record, <Self as [<#relation_name BaseRepository>]>::Id)>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                ;
            } },
            load_by_multiple_ids_fn_def_with_todo: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<(<Self as [<#name BaseRepository>]>::Record, <Self as [<#relation_name BaseRepository>]>::Id)>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                {
                    todo!()
                }
            } },
            get_by_single_id_fn_def: quote! { repositories_paste! {
                fn #get_by_single_id_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _id>]: <Self as [<#relation_name BaseRepository>]>::Id,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<(
                        Entity<
                            #entity_value_pointer<#name>,
                            [<Loaded #name Relations>],
                        >,
                        <Self as [<#name BaseRepository>]>::Id,
                        <Self as [<#relation_name BaseRepository>]>::Id,
                    )>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_records: Vec<_> = Self::#load_by_multiple_ids_fn_name(vec![[<#relation_singular_snake_name _id>].clone()], client)?
                        .into_iter()
                        .map(|(adaptor_record, _)| adaptor_record)
                        .collect();

                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());

                    let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                            #all_load_ops_in_multiple
                        }
                    )*

                    Ok(
                        repositories_izip!(rc_records.into_iter(), adaptor_records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(rc_record, adaptor_record, loaded_relations)| (
                                Entity {
                                    value: rc_record,
                                    relations: loaded_relations,
                                },
                                adaptor_record.into(),
                                [<#relation_singular_snake_name _id>].clone(),
                            ))
                            .collect()
                    )
                }
            } },
            get_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #get_by_multiple_ids_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<(
                        Entity<
                            #entity_value_pointer<#name>,
                            [<Loaded #name Relations>]
                        >,
                        <Self as [<#name BaseRepository>]>::Id,
                        <Self as [<#relation_name BaseRepository>]>::Id,
                    )>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_records_and_parent_ids = Self::#load_by_multiple_ids_fn_name([<#relation_singular_snake_name _ids>].clone(), client)?;

                    let (adaptor_records, parent_ids) = repositories_transpose_2(adaptor_records_and_parent_ids);
                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());
                    let rc_records_adaptor_records_and_parent_ids: Vec<_> = repositories_izip!(
                        rc_records.into_iter(),
                        adaptor_records.into_iter(),
                        parent_ids.into_iter(),
                    ).collect();

                    let unique_record_ids: Vec<<Self as [<#name BaseRepository>]>::Id> = rc_records_adaptor_records_and_parent_ids
                        .iter()
                        .map(|(_, adaptor_record, _)| adaptor_record.as_ref())
                        .unique()
                        .map(|record_id_ref| record_id_ref.clone())
                        .collect();

                    let mut all_loaded_relations: Vec<_> = (0..unique_record_ids.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<_> = unique_record_ids.clone();
                            #all_load_ops_in_multiple
                        }
                    )*

                    let all_loaded_relations: std::collections::HashMap<_, _> = repositories_izip!(
                        unique_record_ids.into_iter(),
                        all_loaded_relations.into_iter(),
                    ).collect();

                    Ok(
                        rc_records_adaptor_records_and_parent_ids
                            .into_iter()
                            .map(|(rc_record, adaptor_record, parent_id)| (
                                Entity {
                                    relations: all_loaded_relations[adaptor_record.as_ref()].clone(),
                                    value: rc_record,
                                },
                                adaptor_record.into(),
                                parent_id,
                            ))
                            .collect()
                    )
                }
            } },
        },
        Cardinality::AtLeastOne => LoadOpsBy {
            load_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<(<Self as [<#name BaseRepository>]>::Record, <Self as [<#relation_name BaseRepository>]>::Id)>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                ;
            } },
            load_by_multiple_ids_fn_def_with_todo: quote! { repositories_paste! {
                fn #load_by_multiple_ids_fn_name(
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    client: Self::Client<'_>,
                ) -> Result<Vec<(<Self as [<#name BaseRepository>]>::Record, <Self as [<#relation_name BaseRepository>]>::Id)>, Self::Error>
                where
                    Self: [<#relation_name ReadRepository>]
                {
                    todo!()
                }
            } },
            get_by_single_id_fn_def: quote! { repositories_paste! {
                fn #get_by_single_id_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _id>]: <Self as [<#relation_name BaseRepository>]>::Id,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<(
                        Entity<
                            #entity_value_pointer<#name>,
                            [<Loaded #name Relations>]
                        >,
                        <Self as [<#name BaseRepository>]>::Id,
                        <Self as [<#relation_name BaseRepository>]>::Id,
                    )>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_records: Vec<_> = Self::#load_by_multiple_ids_fn_name(vec![[<#relation_singular_snake_name _id>].clone()], client)?
                        .into_iter()
                        .map(|(adaptor_record, _)| adaptor_record)
                        .collect();

                    if adaptor_records.len() == 0 {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());

                    let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                            #all_load_ops_in_multiple
                        }
                    )*

                    Ok(
                        repositories_izip!(rc_records.into_iter(), adaptor_records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(rc_record, adaptor_record, loaded_relations)| (
                                Entity {
                                    value: rc_record,
                                    relations: loaded_relations,
                                },
                                adaptor_record.into(),
                                [<#relation_singular_snake_name _id>].clone(),
                            ))
                            .collect()
                    )
                }
            } },
            get_by_multiple_ids_fn_def: quote! { repositories_paste! {
                fn #get_by_multiple_ids_fn_name(
                    &mut self,
                    [<#relation_singular_snake_name _ids>]: Vec<<Self as [<#relation_name BaseRepository>]>::Id>,
                    load_relations: &[<Load #name Relations>],
                    client: Self::Client<'_>,
                ) -> Result<
                    Vec<(
                        Entity<
                            #entity_value_pointer<#name>,
                            [<Loaded #name Relations>]
                        >,
                        <Self as [<#name BaseRepository>]>::Id,
                        <Self as [<#relation_name BaseRepository>]>::Id,
                    )>,
                    Self::Error,
                >
                where
                    Self: ReadRepository
                {
                    let adaptor_records_and_parent_ids = Self::#load_by_multiple_ids_fn_name([<#relation_singular_snake_name _ids>].clone(), client)?;

                    let unique_parent_ids_found: Vec<_> = adaptor_records_and_parent_ids
                        .iter()
                        .map(|(_, parent_id)| parent_id)
                        .unique()
                        .collect();

                    if unique_parent_ids_found.len() != [<#relation_singular_snake_name _ids>].len() {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (adaptor_records, parent_ids) = repositories_transpose_2(adaptor_records_and_parent_ids);
                    let rc_records = self.[<store_ #snake_name s>](adaptor_records.iter().collect());
                    let rc_records_adaptor_records_and_parent_ids: Vec<_> = repositories_izip!(
                        rc_records.into_iter(),
                        adaptor_records.into_iter(),
                        parent_ids.into_iter(),
                    ).collect();

                    let unique_record_ids: Vec<<Self as [<#name BaseRepository>]>::Id> = rc_records_adaptor_records_and_parent_ids
                        .iter()
                        .map(|(_, adaptor_record, _)| adaptor_record.as_ref())
                        .unique()
                        .map(|record_id_ref| record_id_ref.clone())
                        .collect();

                    let mut all_loaded_relations: Vec<_> = (0..unique_record_ids.len()).map(|_| [<Loaded #name Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#all_relation_snake_names.as_ref() {
                            let [<#snake_name _ids>]: Vec<_> = unique_record_ids.clone();
                            #all_load_ops_in_multiple
                        }
                    )*

                    let all_loaded_relations: std::collections::HashMap<_, _> = repositories_izip!(
                        unique_record_ids.into_iter(),
                        all_loaded_relations.into_iter(),
                    ).collect();

                    Ok(
                        rc_records_adaptor_records_and_parent_ids
                            .into_iter()
                            .map(|(rc_record, adaptor_record, parent_id)| (
                                Entity {
                                    relations: all_loaded_relations[adaptor_record.as_ref()].clone(),
                                    value: rc_record,
                                },
                                adaptor_record.into(),
                                parent_id,
                            ))
                            .collect()
                    )
                }
            } },
        },
    }
}

fn get_op_by_single_id_fn_name(op: &str, relation_cardinality: &Cardinality, snake_name: &Ident, relation_singular_snake_name: &Ident) -> Ident {
    match relation_cardinality {
        Cardinality::One => format_ident!("{}_{}_by_{}_id", op, snake_name, relation_singular_snake_name),
        Cardinality::OneOrNone => format_ident!("try_{}_{}_by_{}_id", op, snake_name, relation_singular_snake_name),
        Cardinality::Many|Cardinality::AtLeastOne => format_ident!("{}_{}s_by_{}_id", op, snake_name, relation_singular_snake_name),
    }
}

fn get_op_by_multiple_ids_fn_name(op: &str, relation_cardinality: &Cardinality, snake_name: &Ident, relation_singular_snake_name: &Ident) -> Ident {
    match relation_cardinality {
        Cardinality::One => format_ident!("{}_{}s_by_{}_ids", op, snake_name, relation_singular_snake_name),
        Cardinality::OneOrNone => format_ident!("try_{}_{}s_by_{}_ids", op, snake_name, relation_singular_snake_name),
        Cardinality::Many|Cardinality::AtLeastOne => format_ident!("{}_{}s_by_{}_ids", op, snake_name, relation_singular_snake_name),
    }
}


fn get_relation_snake_name(relation_cardinality: &Cardinality, relation_singular_snake_name: &Ident) -> Ident {
    match relation_cardinality {
        Cardinality::One|Cardinality::OneOrNone => relation_singular_snake_name.clone(),
        Cardinality::Many|Cardinality::AtLeastOne => format_ident!("{}s", relation_singular_snake_name),
    }
}

fn get_relation_type(relation_cardinality: &Cardinality, relation_name: &Ident, entity_value_pointer: &TokenStream2) -> TokenStream2 {
    match relation_cardinality {
        Cardinality::One => quote! { repositories_paste! {
            Option<Box<Entity<#entity_value_pointer<#relation_name>, [<Loaded #relation_name Relations>]>>>
        } },
        Cardinality::OneOrNone => quote! { repositories_paste! {
            Option<Box<Option<Entity<#entity_value_pointer<#relation_name>, [<Loaded #relation_name Relations>]>>>>
        } },
        Cardinality::AtLeastOne|Cardinality::Many => quote! { repositories_paste! {
            Option<Box<Vec<Entity<#entity_value_pointer<#relation_name>, [<Loaded #relation_name Relations>]>>>>
        } },
    }
}

#[derive(Clone, Debug)]
struct RepositoriesVariables {
    names: Vec<Ident>,
    snake_names: Vec<Ident>,
    id_types: Vec<TokenStream2>,
    relation_names: Vec<Vec<Ident>>,
    relation_snake_names: Vec<Vec<Ident>>,
    relation_types: Vec<Vec<TokenStream2>>,
    load_ops_in_single: Vec<Vec<TokenStream2>>,
    load_ops_in_multiple: Vec<Vec<TokenStream2>>,
    load_by_multiple_ids_fn_defs: Vec<Vec<TokenStream2>>,
    load_by_multiple_ids_fn_defs_with_todos: Vec<Vec<TokenStream2>>,
    get_by_single_id_fn_defs: Vec<Vec<TokenStream2>>,
    get_by_multiple_ids_fn_defs: Vec<Vec<TokenStream2>>,
    from_relation_record_trait_bound: TokenStream2,
    write_names: Vec<Ident>,
    snake_write_names: Vec<Ident>,
    entity_value_pointer: TokenStream2,
}

impl Default for RepositoriesVariables {
    fn default() -> Self {
        RepositoriesVariables {
            names: vec![],
            snake_names: vec![],
            id_types: vec![],
            relation_names: vec![],
            relation_snake_names: vec![],
            relation_types: vec![],
            load_ops_in_single: vec![],
            load_ops_in_multiple: vec![],
            load_by_multiple_ids_fn_defs: vec![],
            load_by_multiple_ids_fn_defs_with_todos: vec![],
            get_by_single_id_fn_defs: vec![],
            get_by_multiple_ids_fn_defs: vec![],
            from_relation_record_trait_bound: quote! {},
            write_names: vec![],
            snake_write_names: vec![],
            entity_value_pointer: quote! {},
        }
    }
}


struct RepositoriesInput {
    syncability: Syncability,
    _brace: token::Brace,
    repository_inputs: Punctuated<RepositoryInput, Token![,]>,
}

impl Parse for RepositoriesInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_braces;
        Ok(RepositoriesInput {
            syncability: Syncability::from(&input.parse()?),
            _brace: braced!(in_braces in input),
            repository_inputs: in_braces.parse_terminated(RepositoryInput::parse)?,
        })
    }
}

struct RepositoryInput {
    entity: Ident,
    mutability: Mutability,
    _paren: token::Paren,
    id_type: Type,
    _brace: token::Brace,
    repository_relation_inputs: Punctuated<RepositoryRelationInput, Token![,]>,
}

impl Parse for RepositoryInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_parens;
        let in_braces;
        Ok(RepositoryInput {
            entity: input.parse()?,
            mutability: Mutability::from(&input.parse()?),
            _paren: parenthesized!(in_parens in input),
            id_type: in_parens.parse()?,
            _brace: braced!(in_braces in input),
            repository_relation_inputs: in_braces.parse_terminated(RepositoryRelationInput::parse)?,
        })
    }
}

struct RepositoryRelationInput {
    snake_singular_related_entity_name: Ident,
    _colon: Token![:],
    _paren: token::Paren,
    related_entity_name: Ident,
    _comma: Token![,],
    relation_cardinality: Cardinality,
}

impl Parse for RepositoryRelationInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_parens;
        Ok(RepositoryRelationInput {
            snake_singular_related_entity_name: input.parse()?,
            _colon: input.parse()?,
            _paren: parenthesized!(in_parens in input),
            related_entity_name: in_parens.parse()?,
            _comma: in_parens.parse()?,
            relation_cardinality: Cardinality::from(&in_parens.parse()?),
        })
    }
}

macro_rules! collect_repository_variables {
    ($item: ident) => {
        {
            let repositories_input = parse_macro_input!($item as RepositoriesInput);

            let mut vars = RepositoriesVariables::default();

            let mut relation_maps: HashMap<Ident, (Ident, HashMap<Ident, (Ident, Cardinality)>)> = HashMap::default();
            let mut from_relation_record_trait_bounds: Vec<Vec<TokenStream2>> = vec![];

            vars.entity_value_pointer = repositories_input.syncability.entity_value_pointer();

            for (i, repository_input) in repositories_input.repository_inputs.into_iter().enumerate() {
                let name = repository_input.entity;
                let snake_name = format_ident!("{}", format!("{}", name).to_case(Case::Snake));

                if let Mutability::RW = repository_input.mutability {
                    vars.write_names.push(name.clone());
                    vars.snake_write_names.push(snake_name.clone());
                }

                let id_type = repository_input.id_type;
                vars.id_types.push(quote! { #id_type });

                relation_maps.insert(name.clone(), (snake_name.clone(), HashMap::default()));
                vars.relation_names.push(vec![]);
                vars.relation_snake_names.push(vec![]);

                for repository_relation_input in repository_input.repository_relation_inputs {
                    relation_maps.get_mut(&name).unwrap().1.insert(
                        repository_relation_input.related_entity_name.clone(),
                        (
                            repository_relation_input.snake_singular_related_entity_name.clone(),
                            repository_relation_input.relation_cardinality,
                        ),
                    );
                    vars.relation_names[i].push(repository_relation_input.related_entity_name);
                }

                vars.names.push(name);
                vars.snake_names.push(snake_name);
            }

            for (i, name) in vars.names.iter().enumerate() {
                let snake_name = &relation_maps[name].0;
                let relation_map = &relation_maps[name].1;

                vars.relation_snake_names.push(vec![]);
                vars.relation_types.push(vec![]);
                vars.load_ops_in_single.push(vec![]);
                vars.load_ops_in_multiple.push(vec![]);
                vars.load_by_multiple_ids_fn_defs.push(vec![]);
                vars.load_by_multiple_ids_fn_defs_with_todos.push(vec![]);
                vars.get_by_single_id_fn_defs.push(vec![]);
                vars.get_by_multiple_ids_fn_defs.push(vec![]);

                from_relation_record_trait_bounds.push(vec![]);

                for relation_name in vars.relation_names[i].iter() {
                    if !relation_maps.contains_key(relation_name) {
                        panic!("found entity {relation_name} as a relation to {name} but not as an entity itself", relation_name = relation_name, name = name);
                    }

                    let relation_singular_snake_name = &relation_map[relation_name].0;
                    let relation_cardinality = &relation_map[relation_name].1;

                    let relation_snake_name = get_relation_snake_name(&relation_cardinality, &relation_singular_snake_name);
                    let relation_type = get_relation_type(&relation_cardinality, &relation_name, &vars.entity_value_pointer);

                    let load_ops = get_load_ops(LoadOpsConfig {
                        name: &name,
                        snake_name: &snake_name,
                        relation_name: &relation_name,
                        relation_snake_name: &relation_snake_name,
                        relation_singular_snake_name: &relation_singular_snake_name,
                        relation_cardinality: &relation_cardinality,
                        entity_value_pointer: &vars.entity_value_pointer,
                    });

                    let from_relation_record_trait_bound = format!(
                        "<Adaptor as [<{}BaseRepository>]>::Record: Into<{}>",
                        relation_name,
                        relation_name,
                    ).parse().unwrap();

                    vars.relation_snake_names[i].push(relation_snake_name);
                    vars.relation_types[i].push(relation_type);

                    vars.load_ops_in_single[i].push(load_ops.in_single);
                    vars.load_ops_in_multiple[i].push(load_ops.in_multiple);

                    from_relation_record_trait_bounds[i].push(from_relation_record_trait_bound);
                }

                for relation_name in vars.relation_names[i].iter() {
                    let relation_singular_snake_name = &relation_map[relation_name].0;

                    if !relation_maps[relation_name].1.contains_key(name) {
                        panic!(
                            "relations must be encoded symetrically; found relation {name} -({relation_cardinality:?})-> {relation_name}, so expected a relation of the form {relation_name} -(?)-> {name} but none was found",
                            name = name,
                            relation_name = relation_name,
                            relation_cardinality = relation_maps[name].1[relation_name].1,
                        );
                    }
                    let load_ops_by = get_load_ops_by(LoadOpsByConfig {
                        name: &name,
                        snake_name: &snake_name,
                        relation_name: &relation_name,
                        relation_singular_snake_name: &relation_singular_snake_name,
                        relation_cardinality: &relation_maps[relation_name].1[name].1,
                        all_relation_snake_names: &vars.relation_snake_names[i],
                        all_load_ops_in_single: &vars.load_ops_in_single[i],
                        all_load_ops_in_multiple: &vars.load_ops_in_multiple[i],
                        entity_value_pointer: &vars.entity_value_pointer,
                    });

                    vars.load_by_multiple_ids_fn_defs[i].push(load_ops_by.load_by_multiple_ids_fn_def);
                    vars.load_by_multiple_ids_fn_defs_with_todos[i].push(load_ops_by.load_by_multiple_ids_fn_def_with_todo);
                    vars.get_by_single_id_fn_defs[i].push(load_ops_by.get_by_single_id_fn_def);
                    vars.get_by_multiple_ids_fn_defs[i].push(load_ops_by.get_by_multiple_ids_fn_def);
                }
            }

            vars.from_relation_record_trait_bound = from_relation_record_trait_bounds
                .into_iter()
                .map(|from_relation_record_trait_bounds|
                    from_relation_record_trait_bounds
                        .into_iter()
                        .map(|from_relation_record_trait_bound| format!("{}", from_relation_record_trait_bound))
                        .collect::<Vec<String>>()
                        .join(",")
                )
                .collect::<Vec<String>>()
                .join(",")
                .parse()
                .unwrap();

            vars
        }
    };
}

#[proc_macro]
pub fn repositories(item: TokenStream) -> TokenStream  {
    let RepositoriesVariables {
        names,
        snake_names,
        id_types,
        relation_names,
        relation_snake_names,
        relation_types,
        load_ops_in_single,
        load_ops_in_multiple,
        load_by_multiple_ids_fn_defs,
        get_by_single_id_fn_defs,
        get_by_multiple_ids_fn_defs,
        from_relation_record_trait_bound,
        write_names,
        snake_write_names,
        entity_value_pointer,
        ..
    } = collect_repository_variables!(item);

    let expr = quote! {
        repositories_paste! {
            use std::ops::Deref as RepositoriesDeref;

            pub trait ReadRepository = #([<#names ReadRepository>])+*;
            pub trait ReadWriteRepository = ReadRepository + #([<#write_names WriteRepository>])+*;

            pub trait RepositoryError {
                fn not_found() -> Self;
            }

            pub trait BaseRepository: Clone + Default + Sized {
                type Client<'a>: Copy;
                type Error: RepositoryError
                    + From<std::sync::PoisonError<std::sync::RwLockReadGuard<'static, ()>>>
                    + From<std::sync::PoisonError<std::sync::RwLockWriteGuard<'static, ()>>>;

                fn read() -> std::sync::LockResult<std::sync::RwLockReadGuard<'static, ()>>;
                fn write() -> std::sync::LockResult<std::sync::RwLockWriteGuard<'static, ()>>;

                #(
                    fn [<#snake_names s>](&self) -> &std::collections::HashMap<<Self as [<#names BaseRepository>]>::Id, #entity_value_pointer<#names>>
                    where
                        Self: ReadRepository
                    ;

                    fn [<#snake_names s_mut>](&mut self) -> &mut std::collections::HashMap<<Self as [<#names BaseRepository>]>::Id, #entity_value_pointer<#names>>
                    where
                        Self: ReadRepository
                    ;

                    fn [<store_ #snake_names s>](&mut self, [<adaptor_ #snake_names s>]: Vec<&<Self as [<#names BaseRepository>]>::Record>)-> Vec<#entity_value_pointer<#names>>
                    where
                        Self: ReadRepository
                    {
                        let [<stored_ #snake_names s>] = self.[<#snake_names s_mut>]();
                        [<adaptor_ #snake_names s>]
                            .into_iter()
                            .map(|[<adaptor_ #snake_names>]| {
                                if ![<stored_ #snake_names s>].contains_key([<adaptor_ #snake_names>].as_ref()) {
                                    let [<#snake_names _id>] = [<adaptor_ #snake_names>].as_ref().clone();
                                    [<stored_ #snake_names s>].insert([<#snake_names _id>].clone(), #entity_value_pointer::new([<adaptor_ #snake_names>].clone().into()));
                                    [<stored_ #snake_names s>].get(&[<#snake_names _id>]).unwrap().clone()
                                } else {
                                    [<stored_ #snake_names s>].get([<adaptor_ #snake_names>].as_ref()).unwrap().clone()
                                }
                            })
                            .collect()
                    }
                )*
            }

            #[derive(Clone, Debug)]
            pub struct Entity<E, R> {
                pub value: E,
                pub relations: R,
            }

            pub trait Transactional {
                type AdaptorError;

                fn with_transaction<T, E, F>(&self, f: F) -> Result<T, E>
                where
                    F: FnOnce() -> Result<T, E>,
                    E: From<Self::AdaptorError>;
            }

            #(
                pub type [<#names Entity>] = Entity<#entity_value_pointer<#names>, [<Loaded #names Relations>]>;

                #[derive(Clone, Debug)]
                pub struct [<Load #names Relations>] {
                    #(pub #relation_snake_names: Option<Box<[<Load #relation_names Relations>]>>),*
                }

                impl Default for [<Load #names Relations>] {
                    fn default() -> Self {
                        [<Load #names Relations>] {
                            #(#relation_snake_names: None),*
                        }
                    }
                }

                impl [<Load #names Relations>] {
                    #(
                        pub fn [<load_ #relation_snake_names>](mut self) -> [<Load #names Relations>] {
                            self.#relation_snake_names = Some(Box::new([<Load #relation_names Relations>]::default()));
                            self
                        }

                        pub fn [<load_ #relation_snake_names _with>](mut self, with_fn: impl FnOnce([<Load #relation_names Relations>]) -> [<Load #relation_names Relations>]) -> [<Load #names Relations>] {
                            self.#relation_snake_names = Some(Box::new(with_fn([<Load #relation_names Relations>]::default())));
                            self
                        }
                    )*
                }

                #[derive(Clone, Debug)]
                pub struct [<Loaded #names Relations>] {
                    #(pub #relation_snake_names: #relation_types),*
                }

                impl Default for [<Loaded #names Relations>] {
                    fn default() -> Self {
                        [<Loaded #names Relations>] {
                            #(#relation_snake_names: None),*
                        }
                    }
                }

                pub trait [<#names BaseRepository>]: BaseRepository {
                    type Id: Sized + Clone + Eq + From<#id_types> + std::hash::Hash + 'static;
                    type Record: Clone + Sized + AsRef<Self::Id> + Into<Self::Id> + Into<#names> + 'static;
                }

                pub trait [<#names ReadRepository>]: [<#names BaseRepository>] {
                    fn [<load_ #snake_names s>]([<#snake_names _ids>]: Vec<<Self as [<#names BaseRepository>]>::Id>, client: Self::Client<'_>) -> Result<Vec<<Self as [<#names BaseRepository>]>::Record>, Self::Error>;
                    fn [<load_all_ #snake_names s>](client: Self::Client<'_>) -> Result<Vec<<Self as [<#names BaseRepository>]>::Record>, Self::Error>;
                    fn [<try_load_ #snake_names s>]([<#snake_names _ids>]: Vec<<Self as [<#names BaseRepository>]>::Id>, client: Self::Client<'_>) -> Result<Vec<Option<<Self as [<#names BaseRepository>]>::Record>>, Self::Error>;

                    fn [<get_ #snake_names>](
                        &mut self,
                        adaptor_record: <Self as [<#names BaseRepository>]>::Record,
                        load_relations: &[<Load #names Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Entity<
                            #entity_value_pointer<#names>,
                            [<Loaded #names Relations>],
                        >,
                        Self::Error,
                    >
                    where
                        Self: ReadRepository
                    {
                        let rc_record = self.[<store_ #snake_names s>](vec![&adaptor_record]).pop().unwrap();

                        let mut loaded_relations: [<Loaded #names Relations>] = [<Loaded #names Relations>]::default();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snake_names.as_ref() {
                                let [<#snake_names _id>] = adaptor_record.as_ref().clone();
                                #load_ops_in_single
                            }
                        )*

                        Ok(Entity {
                            value: rc_record,
                            relations: loaded_relations,
                        })
                    }

                    fn [<get_ #snake_names s>](
                        &mut self,
                        adaptor_records: Vec<<Self as [<#names BaseRepository>]>::Record>,
                        load_relations: &[<Load #names Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<Entity<
                            #entity_value_pointer<#names>,
                            [<Loaded #names Relations>],
                        >>,
                        Self::Error,
                    >
                    where
                        Self: ReadRepository
                    {
                        let rc_records = self.[<store_ #snake_names s>](adaptor_records.iter().collect());

                        let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #names Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snake_names.as_ref() {
                                let [<#snake_names _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                                #load_ops_in_multiple
                            }
                        )*

                        let mut entities: std::collections::HashMap<
                            <Self as [<#names BaseRepository>]>::Id,
                            Vec<Entity<
                                #entity_value_pointer<#names>,
                                [<Loaded #names Relations>]
                            >>,
                        > = std::collections::HashMap::default();

                        let [<#snake_names _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();

                        for (adaptor_record, rc_record, loaded_relations) in repositories_izip!(adaptor_records.into_iter(), rc_records.into_iter(), all_loaded_relations.into_iter()) {
                            if entities.contains_key(adaptor_record.as_ref()) {
                                entities.get_mut(adaptor_record.as_ref()).unwrap().push(Entity {
                                    value: rc_record,
                                    relations: loaded_relations,
                                });
                            } else {
                                entities.insert(adaptor_record.into(), vec![Entity {
                                    value: rc_record,
                                    relations: loaded_relations,
                                }]);
                            }
                        }

                        Ok(
                            [<#snake_names _ids>]
                                .into_iter()
                                .map(|[<#snake_names _id>]|
                                    entities
                                        .get_mut(&[<#snake_names _id>])
                                        .unwrap()
                                        .pop()
                                        .unwrap()
                                )
                                .collect()
                        )
                    }

                    fn [<try_get_ #snake_names>](
                        &mut self,
                        adaptor_record_option: Option<<Self as [<#names BaseRepository>]>::Record>,
                        load_relations: &[<Load #names Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Option<Entity<
                            #entity_value_pointer<#names>,
                            [<Loaded #names Relations>],
                        >>,
                        Self::Error,
                    >
                    where
                        Self: ReadRepository
                    {
                        let adaptor_record = match adaptor_record_option {
                            Some(record) => record,
                            None => return Ok(None),
                        };

                        let rc_record = self.[<store_ #snake_names s>](vec![&adaptor_record]).pop().unwrap();

                        let mut loaded_relations = [<Loaded #names Relations>]::default();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snake_names.as_ref() {
                                let [<#snake_names _id>] = adaptor_record.as_ref().clone();
                                #load_ops_in_single
                            }
                        )*

                        Ok(Some(Entity { value: rc_record, relations: loaded_relations }))
                    }

                    fn [<try_get_ #snake_names s>](
                        &mut self,
                        adaptor_record_options: Vec<Option<<Self as [<#names BaseRepository>]>::Record>>,
                        load_relations: &[<Load #names Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<Option<Entity<
                            #entity_value_pointer<#names>,
                            [<Loaded #names Relations>],
                        >>>,
                        Self::Error,
                    >
                    where
                        Self: ReadRepository
                    {
                        let (adaptor_records, indices_of_existing_records) = repositories_transpose_2::<<Self as [<#names BaseRepository>]>::Record, usize>(
                            adaptor_record_options
                                .into_iter()
                                .enumerate()
                                .filter_map(|(i, record_option)| record_option.map(|record| (record, i)))
                                .collect()
                        );

                        let rc_records = self.[<store_ #snake_names s>](adaptor_records.iter().collect());

                        let mut all_loaded_relations: Vec<_> = (0..rc_records.len()).map(|_| [<Loaded #names Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snake_names.as_ref() {
                                let [<#snake_names _ids>]: Vec<_> = adaptor_records.iter().map(|adaptor_record| adaptor_record.as_ref().clone()).collect();
                                #load_ops_in_multiple
                            }
                        )*

                        let mut entity_options: Vec<Option<Entity<
                            #entity_value_pointer<#names>,
                            [<Loaded #names Relations>],
                        >>> = Vec::with_capacity(adaptor_records.len());

                        let mut option_index = 0;
                        for (i, (rc_record, loaded_relations)) in repositories_izip!(rc_records.into_iter(), all_loaded_relations.into_iter()).enumerate() {
                            let desired_option_index = indices_of_existing_records[i];
                            while option_index < desired_option_index {
                                entity_options.push(None);
                                option_index += 1;
                            }
                            entity_options.push(Some(Entity {
                                value: rc_record,
                                relations: loaded_relations,
                            }));
                        }

                        Ok(entity_options)
                    }

                    #(
                        #load_by_multiple_ids_fn_defs
                        #get_by_single_id_fn_defs
                        #get_by_multiple_ids_fn_defs
                    )*
                }


                pub struct [<Get #names Builder>]<Adaptor: ReadRepository> {
                    adaptor: Adaptor,
                    load_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<<Adaptor as [<#names BaseRepository>]>::Record, <Adaptor as BaseRepository>::Error>>,
                    load_relations: [<Load #names Relations>],
                }

                pub struct [<Get #names sBuilder>]<Adaptor: ReadRepository> {
                    adaptor: Adaptor,
                    num_requested_records: isize,
                    load_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<<Adaptor as [<#names BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                    load_relations: [<Load #names Relations>],
                }

                pub struct [<TryGet #names Builder>]<Adaptor: ReadRepository> {
                    adaptor: Adaptor,
                    try_load_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Option<<Adaptor as [<#names BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                    load_relations: [<Load #names Relations>],
                }

                pub struct [<TryGet #names sBuilder>]<Adaptor: ReadRepository> {
                    adaptor: Adaptor,
                    num_requested_records: isize,
                    try_load_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<<Adaptor as [<#names BaseRepository>]>::Record>>, <Adaptor as BaseRepository>::Error>>,
                    load_relations: [<Load #names Relations>],
                }


                impl<Adaptor: ReadRepository> [<Get #names Builder>]<Adaptor> {
                    #(
                        pub fn [<load_ #relation_snake_names>](mut self) -> [<Get #names Builder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names>]();
                            self
                        }

                        pub fn [<load_ #relation_snake_names _with>](mut self, with_fn: impl FnOnce([<Load #relation_names Relations>]) -> [<Load #relation_names Relations>]) -> [<Get #names Builder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names _with>](with_fn);
                            self
                        }
                    )*
                }

                impl<Adaptor: ReadRepository> [<Get #names sBuilder>]<Adaptor> {
                    #(
                        pub fn [<load_ #relation_snake_names>](mut self) -> [<Get #names sBuilder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names>]();
                            self
                        }

                        pub fn [<load_ #relation_snake_names _with>](mut self, with_fn: impl FnOnce([<Load #relation_names Relations>]) -> [<Load #relation_names Relations>]) -> [<Get #names sBuilder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names _with>](with_fn);
                            self
                        }
                    )*
                }

                impl<Adaptor: ReadRepository> [<TryGet #names Builder>]<Adaptor> {
                    #(
                        pub fn [<load_ #relation_snake_names>](mut self) -> [<TryGet #names Builder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names>]();
                            self
                        }

                        pub fn [<load_ #relation_snake_names _with>](mut self, with_fn: impl FnOnce([<Load #relation_names Relations>]) -> [<Load #relation_names Relations>]) -> [<TryGet #names Builder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names _with>](with_fn);
                            self
                        }
                    )*
                }

                impl<Adaptor: ReadRepository> [<TryGet #names sBuilder>]<Adaptor> {
                    #(
                        pub fn [<load_ #relation_snake_names>](mut self) -> [<TryGet #names sBuilder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names>]();
                            self
                        }

                        pub fn [<load_ #relation_snake_names _with>](mut self, with_fn: impl FnOnce([<Load #relation_names Relations>]) -> [<Load #relation_names Relations>]) -> [<TryGet #names sBuilder>]<Adaptor> {
                            self.load_relations = self.load_relations.[<load_ #relation_snake_names _with>](with_fn);
                            self
                        }
                    )*
                }


                // run
                impl<Adaptor: ReadRepository> [<Get #names Builder>]<Adaptor>
                where
                    #from_relation_record_trait_bound
                {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#entity_value_pointer<#names>, [<Loaded #names Relations>]>, <Adaptor as BaseRepository>::Error> {
                        let _read_lock = Adaptor::read()?;
                        let adaptor_record = (self.load_adaptor_record)(client)?;
                        self.adaptor.[<get_ #snake_names>](adaptor_record, &self.load_relations, client)
                    }
                }

                impl<Adaptor: ReadRepository> [<Get #names sBuilder>]<Adaptor>
                where
                    #from_relation_record_trait_bound
                {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#entity_value_pointer<#names>, [<Loaded #names Relations>]>>, <Adaptor as BaseRepository>::Error> {
                        let _read_lock = Adaptor::read()?;
                        let adaptor_records = (self.load_adaptor_records)(client)?;
                        if self.num_requested_records >= 0 && adaptor_records.len() as isize != self.num_requested_records {
                            return Err(<<Adaptor as BaseRepository>::Error as RepositoryError>::not_found());
                        }
                        self.adaptor.[<get_ #snake_names s>](adaptor_records, &self.load_relations, client)
                    }
                }

                impl<Adaptor: ReadRepository> [<TryGet #names Builder>]<Adaptor>
                where
                    #from_relation_record_trait_bound
                {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Option<Entity<#entity_value_pointer<#names>, [<Loaded #names Relations>]>>, <Adaptor as BaseRepository>::Error> {
                        let _read_lock = Adaptor::read()?;
                        let adaptor_record_option = (self.try_load_adaptor_record)(client)?;
                        self.adaptor.[<try_get_ #snake_names>](adaptor_record_option, &self.load_relations, client)
                    }
                }

                impl<Adaptor: ReadRepository> [<TryGet #names sBuilder>]<Adaptor>
                where
                    #from_relation_record_trait_bound
                {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<Entity<#entity_value_pointer<#names>, [<Loaded #names Relations>]>>>, <Adaptor as BaseRepository>::Error> {
                        let _read_lock = Adaptor::read()?;
                        let adaptor_record_options = (self.try_load_adaptor_records)(client)?;
                        if self.num_requested_records >= 0 && adaptor_record_options.len() as isize != self.num_requested_records {
                            return Err(<<Adaptor as BaseRepository>::Error as RepositoryError>::not_found());
                        }
                        self.adaptor.[<try_get_ #snake_names s>](adaptor_record_options, &self.load_relations, client)
                    }
                }


                impl #names {
                    pub fn get<Adaptor: ReadRepository>(id: <Adaptor as [<#names BaseRepository>]>::Id) -> [<Get #names Builder>]<Adaptor> {
                        [<Get #names Builder>] {
                            adaptor: Adaptor::default(),
                            load_adaptor_record: Box::new(move |client| Ok(Adaptor::[<load_ #snake_names s>](vec![id], client)?.pop().unwrap())),
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn get_batch<Adaptor: ReadRepository>(ids: Vec<<Adaptor as [<#names BaseRepository>]>::Id>) -> [<Get #names sBuilder>]<Adaptor> {
                        [<Get #names sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: ids.len() as isize,
                            load_adaptor_records: Box::new(move |client| Adaptor::[<load_ #snake_names s>](ids, client)),
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn try_get<Adaptor: ReadRepository>(id: <Adaptor as [<#names BaseRepository>]>::Id) -> [<TryGet #names Builder>]<Adaptor> {
                        [<TryGet #names Builder>] {
                            adaptor: Adaptor::default(),
                            try_load_adaptor_record: Box::new(move |client| Ok(Adaptor::[<try_load_ #snake_names s>](vec![id], client)?.pop().unwrap())),
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn try_get_batch<Adaptor: ReadRepository>(ids: Vec<<Adaptor as [<#names BaseRepository>]>::Id>) -> [<TryGet #names sBuilder>]<Adaptor> {
                        [<TryGet #names sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: ids.len() as isize,
                            try_load_adaptor_records: Box::new(move |client| Adaptor::[<try_load_ #snake_names s>](ids, client)),
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn get_all<Adaptor: ReadRepository>() -> [<Get #names sBuilder>]<Adaptor> {
                        [<Get #names sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: -1,
                            load_adaptor_records: Box::new(move |client| Adaptor::[<load_all_ #snake_names s>](client)),
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }


                    pub fn load_into<Adaptor: ReadRepository>(
                        get_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<<Adaptor as [<#names BaseRepository>]>::Record, <Adaptor as BaseRepository>::Error>>,
                    ) -> [<Get #names Builder>]<Adaptor> {
                        [<Get #names Builder>] {
                            adaptor: Adaptor::default(),
                            load_adaptor_record: get_adaptor_record,
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn load_into_batch<Adaptor: ReadRepository>(
                        num_requested_records: usize,
                        get_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<<Adaptor as [<#names BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                    ) -> [<Get #names sBuilder>]<Adaptor> {
                        [<Get #names sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: num_requested_records as isize,
                            load_adaptor_records: get_adaptor_records,
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn load_into_option<Adaptor: ReadRepository>(
                        get_adaptor_record_option: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Option<<Adaptor as [<#names BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                    ) -> [<TryGet #names Builder>]<Adaptor> {
                        [<TryGet #names Builder>] {
                            adaptor: Adaptor::default(),
                            try_load_adaptor_record: get_adaptor_record_option,
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }

                    pub fn load_into_option_batch<Adaptor: ReadRepository>(
                        num_requested_records: usize,
                        get_adaptor_record_options: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<<Adaptor as [<#names BaseRepository>]>::Record>>, <Adaptor as BaseRepository>::Error>>,
                    ) -> [<TryGet #names sBuilder>]<Adaptor> {
                        [<TryGet #names sBuilder>] {
                            adaptor: Adaptor::default(),
                            num_requested_records: num_requested_records as isize,
                            try_load_adaptor_records: get_adaptor_record_options,
                            load_relations: [<Load #names Relations>]::default(),
                        }
                    }
                }
            )*

            #(
                pub trait [<#write_names WriteRepository>]: [<#write_names ReadRepository>] {
                    type RecordPost: From<[<#write_names Post>]>;
                    type RecordPatch: From<[<#write_names Patch>]>;

                    fn [<delete_ #snake_write_names s>](
                        [<#snake_write_names _ids>]: Vec<<Self as [<#write_names BaseRepository>]>::Id>,
                        client: Self::Client<'_>,
                    ) -> Result<usize, Self::Error>;

                    fn [<insert_ #snake_write_names s>](
                        [<#snake_write_names _posts>]: Vec<<Self as [<#write_names WriteRepository>]>::RecordPost>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<<Self as [<#write_names BaseRepository>]>::Record>, Self::Error>;

                    fn [<update_ #snake_write_names s>](
                        [<#snake_write_names _patches>]: Vec<<Self as [<#write_names WriteRepository>]>::RecordPatch>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<<Self as [<#write_names BaseRepository>]>::Record>, Self::Error>;
                }


                pub struct [<Delete #write_names Builder>]<Adaptor: [<#write_names WriteRepository>]> {
                    id: <Adaptor as [<#write_names BaseRepository>]>::Id,
                }

                pub struct [<Delete #write_names sBuilder>]<Adaptor: [<#write_names WriteRepository>]> {
                    ids: Vec<<Adaptor as [<#write_names BaseRepository>]>::Id>,
                }

                pub struct [<Insert #write_names Builder>]<Adaptor: [<#write_names WriteRepository>] + ReadRepository> {
                    adaptor: Adaptor,
                    post: [<#write_names Post>],
                    load_relations: [<Load #write_names Relations>],
                }

                pub struct [<Insert #write_names sBuilder>]<Adaptor: [<#write_names WriteRepository>] + ReadRepository> {
                    adaptor: Adaptor,
                    posts: Vec<[<#write_names Post>]>,
                    load_relations: [<Load #write_names Relations>],
                }

                pub struct [<Update #write_names Builder>]<Adaptor: [<#write_names WriteRepository>] + ReadRepository> {
                    adaptor: Adaptor,
                    patch: [<#write_names Patch>],
                    load_relations: [<Load #write_names Relations>],
                }

                pub struct [<Update #write_names sBuilder>]<Adaptor: [<#write_names WriteRepository>] + ReadRepository> {
                    adaptor: Adaptor,
                    patches: Vec<[<#write_names Patch>]>,
                    load_relations: [<Load #write_names Relations>],
                }


                impl<Adaptor: [<#write_names WriteRepository>]> [<Delete #write_names Builder>]<Adaptor> {
                    pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                        let _write_lock = Adaptor::write()?;
                        Adaptor::[<delete_ #snake_write_names s>](vec![self.id], client)
                    }
                }

                impl<Adaptor: [<#write_names WriteRepository>]> [<Delete #write_names sBuilder>]<Adaptor> {
                    pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                        let _write_lock = Adaptor::write()?;
                        Adaptor::[<delete_ #snake_write_names s>](self.ids, client)
                    }
                }


                impl<Adaptor: [<#write_names WriteRepository>] + ReadRepository> [<Insert #write_names Builder>]<Adaptor> {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#entity_value_pointer<#write_names>, [<Loaded #write_names Relations>]>, <Adaptor as BaseRepository>::Error> {
                        let adaptor_record = {
                            let _write_lock = Adaptor::write()?;
                            let adaptor_post = <Adaptor as [<#write_names WriteRepository>]>::RecordPost::from(self.post);
                            Adaptor::[<insert_ #snake_write_names s>](vec![adaptor_post], client)?.pop().unwrap()
                        };
                        let _read_lock = Adaptor::read()?;
                        self.adaptor.[<get_ #snake_write_names>](adaptor_record, &self.load_relations, client)
                    }
                }

                impl<Adaptor: [<#write_names WriteRepository>] + ReadRepository> [<Insert #write_names sBuilder>]<Adaptor> {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#entity_value_pointer<#write_names>, [<Loaded #write_names Relations>]>>, <Adaptor as BaseRepository>::Error> {
                        let adaptor_records = {
                            let _write_lock = Adaptor::write()?;
                            let adaptor_posts = self.posts.into_iter().map(<Adaptor as [<#write_names WriteRepository>]>::RecordPost::from).collect();
                            Adaptor::[<insert_ #snake_write_names s>](adaptor_posts, client)?
                        };
                        let _read_lock = Adaptor::read()?;
                        self.adaptor.[<get_ #snake_write_names s>](adaptor_records, &self.load_relations, client)
                    }
                }


                impl<Adaptor: [<#write_names WriteRepository>] + ReadRepository> [<Update #write_names Builder>]<Adaptor> {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#entity_value_pointer<#write_names>, [<Loaded #write_names Relations>]>, <Adaptor as BaseRepository>::Error> {
                        let adaptor_record = {
                            let _write_lock = Adaptor::write()?;
                            let adaptor_patch = <Adaptor as [<#write_names WriteRepository>]>::RecordPatch::from(self.patch);
                            Adaptor::[<update_ #snake_write_names s>](vec![adaptor_patch], client)?.pop().unwrap()
                        };
                        let _read_lock = Adaptor::read()?;
                        self.adaptor.[<get_ #snake_write_names>](adaptor_record, &self.load_relations, client)
                    }
                }

                impl<Adaptor: [<#write_names WriteRepository>] + ReadRepository> [<Update #write_names sBuilder>]<Adaptor> {
                    pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#entity_value_pointer<#write_names>, [<Loaded #write_names Relations>]>>, <Adaptor as BaseRepository>::Error> {
                        let adaptor_records = {
                            let _write_lock = Adaptor::write()?;
                            let adaptor_patches = self.patches.into_iter().map(<Adaptor as [<#write_names WriteRepository>]>::RecordPatch::from).collect();
                            Adaptor::[<update_ #snake_write_names s>](adaptor_patches, client)?
                        };
                        let _read_lock = Adaptor::read()?;
                        self.adaptor.[<get_ #snake_write_names s>](adaptor_records, &self.load_relations, client)
                    }
                }


                impl #write_names {
                    pub fn delete<Adaptor: [<#write_names WriteRepository>]>(id: <Adaptor as [<#write_names BaseRepository>]>::Id) -> [<Delete #write_names Builder>]<Adaptor> {
                        [<Delete #write_names Builder>] {
                            id,
                        }
                    }

                    pub fn delete_batch<Adaptor: [<#write_names WriteRepository>]>(ids: Vec<<Adaptor as [<#write_names BaseRepository>]>::Id>) -> [<Delete #write_names sBuilder>]<Adaptor> {
                        [<Delete #write_names sBuilder>] {
                            ids,
                        }
                    }

                    pub fn insert<Adaptor: [<#write_names WriteRepository>] + ReadRepository>(post: [<#write_names Post>]) -> [<Insert #write_names Builder>]<Adaptor> {
                        [<Insert #write_names Builder>] {
                            adaptor: Adaptor::default(),
                            post,
                            load_relations: [<Load #write_names Relations>]::default(),
                        }
                    }

                    pub fn insert_batch<Adaptor: [<#write_names WriteRepository>] + ReadRepository>(posts: Vec<[<#write_names Post>]>) -> [<Insert #write_names sBuilder>]<Adaptor> {
                        [<Insert #write_names sBuilder>] {
                            adaptor: Adaptor::default(),
                            posts,
                            load_relations: [<Load #write_names Relations>]::default(),
                        }
                    }

                    pub fn update<Adaptor: [<#write_names WriteRepository>] + ReadRepository>(patch: [<#write_names Patch>]) -> [<Update #write_names Builder>]<Adaptor> {
                        [<Update #write_names Builder>] {
                            adaptor: Adaptor::default(),
                            patch,
                            load_relations: [<Load #write_names Relations>]::default(),
                        }
                    }

                    pub fn update_batch<Adaptor: [<#write_names WriteRepository>] + ReadRepository>(patches: Vec<[<#write_names Patch>]>) -> [<Update #write_names sBuilder>]<Adaptor> {
                        [<Update #write_names sBuilder>] {
                            adaptor: Adaptor::default(),
                            patches,
                            load_relations: [<Load #write_names Relations>]::default(),
                        }
                    }
                }
            )*
        }
    };

    expr.into()
}

#[proc_macro]
pub fn print_repositories(item: TokenStream) -> TokenStream {
    let RepositoriesVariables {
        names,
        snake_names,
        id_types,
        load_by_multiple_ids_fn_defs_with_todos,
        write_names,
        snake_write_names,
        entity_value_pointer,
        ..
    } = collect_repository_variables!(item);

    let expr = quote! {
        repositories_paste! {
            pub trait BaseRepository: Clone + Sized {
                type Client<'a>: Copy;
                type Error:
                    From<std::sync::PoisonError<std::sync::RwLockReadGuard<'static, ()>>>
                    + From<std::sync::PoisonError<std::sync::RwLockWriteGuard<'static, ()>>>;

                fn read() -> std::sync::LockResult<std::sync::RwLockReadGuard<'static, ()>>;
                fn write() -> std::sync::LockResult<std::sync::RwLockWriteGuard<'static, ()>>;

                #(
                    fn [<#snake_names s>](&self) -> &std::collections::HashMap<<Self as [<#names BaseRepository>]>::Id, #entity_value_pointer<<Self as [<#names BaseRepository>]>::Record>>
                    where
                        Self: ReadRepository
                    ;

                    fn [<#snake_names s_mut>](&mut self) -> &mut std::collections::HashMap<<Self as [<#names BaseRepository>]>::Id, #entity_value_pointer<<Self as [<#names BaseRepository>]>::Record>>
                    where
                        Self: ReadRepository
                    ;
                )*
            }
            #(
                pub trait [<#names BaseRepository>]: BaseRepository {
                    type Id: Sized + Clone + Eq + From<#id_types> + std::hash::Hash;
                    type Record: Clone + Sized + Into<#names> + AsRef<Self::Id>;
                }

                pub trait [<#names ReadRepository>]: [<#names BaseRepository>] {
                    fn [<load_ #snake_names s>]([<#snake_names _ids>]: Vec<<Self as [<#names BaseRepository>]>::Id>, client: Self::Client<'_>) -> Result<Vec<<Self as [<#names BaseRepository>]>::Record>, Self::Error> {
                        todo!()
                    }

                    fn [<load_all_ #snake_names s>](client: Self::Client<'_>) -> Result<Vec<<Self as [<#names BaseRepository>]>::Record>, Self::Error> {
                        todo!()
                    }

                    fn [<try_load_ #snake_names s>]([<#snake_names _ids>]: Vec<<Self as [<#names BaseRepository>]>::Id>, client: Self::Client<'_>) -> Result<Vec<Option<<Self as [<#names BaseRepository>]>::Record>>, Self::Error> {
                        todo!()
                    }

                    #(
                        #load_by_multiple_ids_fn_defs_with_todos
                    )*
                }
            )*
            #(
                pub trait [<#write_names WriteRepository>]: [<#write_names ReadRepository>] {
                    type RecordPost: From<[<#write_names Post>]>;
                    type RecordPatch: From<[<#write_names Patch>]>;

                    fn [<delete_ #snake_write_names s>](
                        [<#snake_write_names _ids>]: Vec<<Self as [<#write_names BaseRepository>]>::Id>,
                        client: Self::Client<'_>,
                    ) -> Result<usize, Self::Error> {
                        todo!()
                    }

                    fn [<insert_ #snake_write_names s>](
                        [<#snake_write_names _posts>]: Vec<<Self as [<#write_names WriteRepository>]>::RecordPost>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<<Self as [<#write_names BaseRepository>]>::Record>, Self::Error> {
                        todo!()
                    }

                    fn [<update_ #snake_write_names s>](
                        [<#snake_write_names _patches>]: Vec<<Self as [<#write_names WriteRepository>]>::RecordPatch>,
                        client: Self::Client<'_>,
                    ) -> Result<Vec<<Self as [<#write_names BaseRepository>]>::Record>, Self::Error> {
                        todo!()
                    }
                }
            )*
        }
    };

    expr.into()
}

struct LoadByInput {
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

#[proc_macro]
pub fn load_by(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as LoadByInput);

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

#[proc_macro]
pub fn print_load_by(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as LoadByInput);

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
                    fn [<load_ #snake_name s_by_ #field_names s>]([<#field_names s>]: Vec<#field_types>, client: Self::Client<'_>) -> Result<Vec<<Self as [<#name BaseRepository>]>::Record>, Self::Error> {
                        todo!()
                    }
                )*
            }
        }
    };

    expr.into()
}
