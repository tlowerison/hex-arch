use crate::input::*;

use proc_macro2::TokenStream as TokenStream2;
use syn::Ident;

pub (crate) fn repositories(input: RepositoriesInput, todo: bool) -> TokenStream2 {
    let read_repositories = read_repositories::read_repositories(&input, todo);
    let write_repositories = write_repositories::write_repositories(&input);

    quote! {
        use std::ops::Deref as RepositoriesDeref;

        #read_repositories
        #write_repositories
    }
}

pub (crate) mod read_repositories {
    use super::*;

    pub (crate) fn read_repositories(input: &RepositoriesInput, todo: bool) -> TokenStream2 {
        let read_repositories: Vec<_> = input.repositories
            .iter()
            .map(|repository| {
                let ty_entity = ty_entity(repository);
                let load_ty_relations = load_ty_relations(repository);
                let loaded_ty_relations = loaded_ty_relations(repository);
                let ty_base_repository = ty_base_repository(repository);
                let ty_read_repository = ty_read_repository::ty_read_repository(repository, input, todo);
                let get_ty_singular_builder = builders::get_ty_singular_builder(repository);
                let get_ty_multiple_builder = builders::get_ty_multiple_builder(repository);
                let try_get_ty_singular_builder = builders::try_get_ty_singular_builder(repository);
                let try_get_ty_multiple_builder = builders::try_get_ty_multiple_builder(repository);
                let get_by_fields: Vec<_> = repository
                    .load_bys
                    .iter()
                    .map(|load_by| ty_read_repository::get_by_field(repository, load_by))
                    .collect();

                quote! {
                    #ty_entity
                    #load_ty_relations
                    #loaded_ty_relations
                    #ty_base_repository
                    #ty_read_repository
                    #get_ty_singular_builder
                    #get_ty_multiple_builder
                    #try_get_ty_singular_builder
                    #try_get_ty_multiple_builder
                    #(#get_by_fields)*
                }
            })
            .collect();

        quote! {
            #(#read_repositories)*
        }
    }

    fn ty_entity(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let sync_ptr = repository.sync_ptr();
        quote! {
            repositories_paste! {
                pub type [<#ty Entity>] = Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>;
            }
        }
    }

    fn load_ty_relations(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let relation_tys = repository.relation_tys();
        let relation_snakes = repository.relation_snakes();

        quote! {
            repositories_paste! {
                #[derive(Clone, Debug)]
                pub struct [<Load #ty Relations>] {
                    #(pub #relation_snakes: Option<Box<[<Load #relation_tys Relations>]>>),*
                }

                impl Default for [<Load #ty Relations>] {
                    fn default() -> Self {
                        [<Load #ty Relations>] {
                            #(#relation_snakes: None),*
                        }
                    }
                }

                impl [<Load #ty Relations>] {
                    #(
                        pub fn [<load_ #relation_snakes>](mut self) -> [<Load #ty Relations>] {
                            self.#relation_snakes = Some(Box::new([<Load #relation_tys Relations>]::default()));
                            self
                        }

                        pub fn [<load_ #relation_snakes _with>](mut self, with_fn: impl FnOnce([<Load #relation_tys Relations>]) -> [<Load #relation_tys Relations>]) -> [<Load #ty Relations>] {
                            self.#relation_snakes = Some(Box::new(with_fn([<Load #relation_tys Relations>]::default())));
                            self
                        }
                    )*
                }
            }
        }
    }

    fn loaded_ty_relations(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let relation_snakes = repository.relation_snakes();
        let relation_types: Vec<_> = repository.relations
            .iter()
            .map(loaded_relation_type)
            .collect();

        quote! {
            repositories_paste! {
                #[derive(Clone, Debug)]
                pub struct [<Loaded #ty Relations>] {
                    #(pub #relation_snakes: #relation_types),*
                }

                impl Default for [<Loaded #ty Relations>] {
                    fn default() -> Self {
                        [<Loaded #ty Relations>] {
                            #(#relation_snakes: None),*
                        }
                    }
                }
            }
        }
    }

    fn loaded_relation_type(relation: &RelationInput) -> TokenStream2 {
        let ty = relation.ty();
        let sync_ptr = relation.sync_ptr();

        match relation.cardinality {
            Cardinality::One => quote! {
                repositories_paste! {
                    Option<Box<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>
                }
            },
            Cardinality::OneOrNone => quote! {
                repositories_paste! {
                    Option<Box<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>>
                }
            },
            Cardinality::AtLeastOne|Cardinality::Many => quote! {
                repositories_paste! {
                    Option<Box<Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>>
                }
            },
        }
    }

    fn ty_base_repository(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let id_type = &repository.id_type;
        quote! {
            repositories_paste! {
                pub trait [<#ty BaseRepository>]: BaseRepository {
                    type Id: Sized + Clone + Eq + From<#id_type> + PartialEq + std::hash::Hash + 'static;
                    type Record: Clone + Sized + AsRef<Self::Id> + Into<Self::Id> + Into<#ty> + 'static;
                }
            }
        }
    }

    pub (crate) mod ty_read_repository {
        use super::*;
        use shared::*;

        pub (crate) fn ty_read_repository(repository: &RepositoryInput, input: &RepositoriesInput, todo: bool) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let get_singular = get_singular(repository);
            let get_multiple = get_multiple(repository);
            let try_get_singular = try_get_singular(repository);
            let try_get_multiple = try_get_multiple(repository);

            let entity_storage = entity_storage(repository);

            let inward_relations: Vec<_> = input.repositories
                .iter()
                .map(|rep|
                    rep.relations
                        .iter()
                        .filter_map(|relation|
                            if *relation.ty() == *repository.ty() {
                                Some((relation.clone(), rep.clone()))
                            } else {
                                None
                            }
                        )
                        .collect::<Vec<_>>()
                )
                .flatten()
                .collect();

            let load_by_multiples: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| load_by_multiple(relation, repository, relation_repository, todo))
                .collect();
            let get_by_singles: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| get_by_single(relation, repository, relation_repository))
                .collect();
            let get_by_multiples: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| get_by_multiple(relation, repository, relation_repository))
                .collect();

            let load_by_field_multiples: Vec<_> = repository
                .load_bys
                .iter()
                .map(|load_by| load_by_field_multiple(repository, load_by, todo))
                .collect();

            quote! {
                repositories_paste! {
                    pub trait [<#ty ReadRepository>]: [<#ty BaseRepository>] {
                        #entity_storage

                        fn [<load_ #plural>](
                            [<#singular _ids>]: Vec<<Self as [<#ty BaseRepository>]>::Id>,
                            client: Self::Client<'_>,
                        ) -> Result<
                            Vec<<Self as [<#ty BaseRepository>]>::Record>,
                            Self::Error,
                        >;

                        fn [<load_all_ #plural>](
                            client: Self::Client<'_>,
                        ) -> Result<
                            Vec<<Self as [<#ty BaseRepository>]>::Record>,
                            Self::Error,
                        >;

                        fn [<try_load_ #plural>](
                            [<#singular _ids>]: Vec<<Self as [<#ty BaseRepository>]>::Id>,
                            client: Self::Client<'_>,
                        ) -> Result<
                            Vec<Option<<Self as [<#ty BaseRepository>]>::Record>>,
                            Self::Error,
                        >;

                        #get_singular
                        #get_multiple
                        #try_get_singular
                        #try_get_multiple
                        #(
                            #load_by_multiples
                            #get_by_singles
                            #get_by_multiples
                        )*

                        #(
                            #load_by_field_multiples
                        )*
                    }
                }
            }
        }

        fn entity_storage(repository: &RepositoryInput) -> TokenStream2 {
            let sync_ptr = repository.sync_ptr();
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    fn [<#plural _mut>](&mut self) -> &mut std::collections::HashMap<<Self as [<#ty BaseRepository>]>::Id, (#sync_ptr<#ty>, #sync_ptr<<Self as [<#ty BaseRepository>]>::Record>)>
                    where
                        Self: #read_repositories
                    ;

                    fn [<foo_store_ #singular>](&mut self, #singular: <Self as [<#ty BaseRepository>]>::Record)-> (#sync_ptr<#ty>, #sync_ptr<<Self as [<#ty BaseRepository>]>::Record>)
                    where
                        Self: #read_repositories
                    {
                        let (mut records, mut #plural) = self.[<store_ #plural>](vec![#singular]);
                        (records.pop().unwrap(), #plural.pop().unwrap())
                    }

                    fn [<store_ #plural>](&mut self, [<adaptor_ #plural>]: Vec<<Self as [<#ty BaseRepository>]>::Record>)-> (Vec<#sync_ptr<#ty>>, Vec<#sync_ptr<<Self as [<#ty BaseRepository>]>::Record>>)
                    where
                        Self: #read_repositories
                    {
                        let [<stored_ #plural>] = self.[<#plural _mut>]();
                        repositories_transpose_2(
                            [<adaptor_ #plural>]
                                .into_iter()
                                .map(|[<adaptor_ #singular>]| {
                                    let id = [<adaptor_ #singular>].as_ref();
                                    let record_and_adaptor_record = if ![<stored_ #plural>].contains_key(id) {
                                        let adaptor_record = [<adaptor_ #singular>].clone();
                                        [<stored_ #plural>]
                                            .entry(id.clone())
                                            .or_insert((
                                                #sync_ptr::new([<adaptor_ #singular>].into()),
                                                #sync_ptr::new(adaptor_record),
                                            ))
                                    } else {
                                        [<stored_ #plural>].get(id).unwrap()
                                    };
                                    (record_and_adaptor_record.0.clone(), record_and_adaptor_record.1.clone())
                                })
                                .collect()
                        )
                    }
                }
            }
        }

        pub (crate) fn get_singular(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let load_in_singles: Vec<_> = repository.relations
                .iter()
                .map(|relation| shared::load_in_single(repository, relation))
                .collect();

            quote! {
                repositories_paste! {
                    fn [<get_ #singular>](
                        &mut self,
                        #singular: <Self as [<#ty BaseRepository>]>::Record,
                        load_relations: &[<Load #ty Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        let (record, #singular) = self.[<foo_store_ #singular>](#singular);
                        let #singular = #singular.deref();

                        let mut loaded_relations: [<Loaded #ty Relations>] = [<Loaded #ty Relations>]::default();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                                #load_in_singles
                            }
                        )*

                        Ok(Entity {
                            value: record,
                            relations: loaded_relations,
                        })
                    }
                }
            }
        }

        pub (crate) fn get_multiple(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let load_in_multiples: Vec<_> = repository.relations
                .iter()
                .map(|relation| shared::load_in_multiple(repository, relation))
                .collect();

            quote! {
                repositories_paste! {
                    fn [<get_ #plural>](
                        &mut self,
                        #plural: Vec<<Self as [<#ty BaseRepository>]>::Record>,
                        load_relations: &[<Load #ty Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](#plural);
                        let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                        let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                                #load_in_multiples
                            }
                        )*

                        let mut entities: std::collections::HashMap<
                            <Self as [<#ty BaseRepository>]>::Id,
                            Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>,
                        > = std::collections::HashMap::default();

                        let [<#singular _ids>]: Vec<<Self as [<#ty BaseRepository>]>::Id> = records
                            .iter()
                            .map(|record| record.deref().as_ref().clone().into())
                            .collect();

                        for (record, loaded_relations) in repositories_izip!(records.into_iter(), all_loaded_relations.into_iter()) {
                            let record_id: <Self as [<#ty BaseRepository>]>::Id = record.deref().as_ref().clone().into();
                            if entities.contains_key(&record_id) {
                                entities.get_mut(&record_id).unwrap().push(Entity {
                                    value: record,
                                    relations: loaded_relations,
                                });
                            } else {
                                entities.insert(record_id, vec![Entity {
                                    value: record,
                                    relations: loaded_relations,
                                }]);
                            }
                        }

                        Ok(
                            [<#singular _ids>]
                                .into_iter()
                                .map(|[<#singular _id>]|
                                    entities
                                        .get_mut(&[<#singular _id>])
                                        .unwrap()
                                        .pop()
                                        .unwrap()
                                )
                                .collect()
                        )
                    }
                }
            }
        }

        pub (crate) fn try_get_singular(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let load_in_singles: Vec<_> = repository.relations
                .iter()
                .map(|relation| shared::load_in_single(repository, relation))
                .collect();

            quote! {
                repositories_paste! {
                    fn [<try_get_ #singular>](
                        &mut self,
                        adaptor_record_option: Option<<Self as [<#ty BaseRepository>]>::Record>,
                        load_relations: &[<Load #ty Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        let #singular = match adaptor_record_option {
                            Some(record) => record,
                            None => return Ok(None),
                        };

                        let (record, #singular) = self.[<foo_store_ #singular>](#singular);
                        let #singular = #singular.deref();

                        let mut loaded_relations = [<Loaded #ty Relations>]::default();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                                #load_in_singles
                            }
                        )*

                        Ok(Some(Entity {
                            value: record,
                            relations: loaded_relations,
                        }))
                    }
                }
            }
        }

        pub (crate) fn try_get_multiple(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let load_in_multiples: Vec<_> = repository.relations
                .iter()
                .map(|relation| shared::load_in_multiple(repository, relation))
                .collect();

            quote! {
                repositories_paste! {
                    fn [<try_get_ #plural>](
                        &mut self,
                        adaptor_record_options: Vec<Option<<Self as [<#ty BaseRepository>]>::Record>>,
                        load_relations: &[<Load #ty Relations>],
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        let (#plural, indices_of_existing_records) = repositories_transpose_2::<<Self as [<#ty BaseRepository>]>::Record, usize>(
                            adaptor_record_options
                                .into_iter()
                                .enumerate()
                                .filter_map(|(i, record_option)| record_option.map(|record| (record, i)))
                                .collect()
                        );

                        let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](#plural);
                        let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                        let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                                #load_in_multiples
                            }
                        )*

                        let mut entity_options: Vec<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>> = Vec::with_capacity(records.len());

                        let mut option_index = 0;
                        for (i, (record, loaded_relations)) in repositories_izip!(records.into_iter(), all_loaded_relations.into_iter()).enumerate() {
                            let desired_option_index = indices_of_existing_records[i];
                            while option_index < desired_option_index {
                                entity_options.push(None);
                                option_index += 1;
                            }
                            entity_options.push(Some(Entity {
                                value: record,
                                relations: loaded_relations,
                            }));
                        }

                        Ok(entity_options)
                    }
                }
            }
        }

        pub (crate) fn load_by_multiple(relation: &RelationInput, repository: &RepositoryInput, relation_repository: &RepositoryInput, todo: bool) -> TokenStream2 {
            let ty = repository.ty();
            let relation_ty = relation_repository.ty();
            let relation_plural = relation_repository.plural();
            let load_by_multiple_fn_name = op_by_multiple_fn_name("load", relation, relation_repository);

            let body = if todo { quote! { { todo!() } } } else { quote! { ; } };

            let return_ty = match relation.cardinality {
                Cardinality::One => { quote! { repositories_paste! {
                    Vec<<Self as [<#ty BaseRepository>]>::Record>
                } } },
                Cardinality::OneOrNone => { quote! { repositories_paste! {
                    Vec<Option<<Self as [<#ty BaseRepository>]>::Record>>
                } } },
                Cardinality::Many|Cardinality::AtLeastOne => { quote! { repositories_paste! {
                    Vec<(
                        <Self as [<#ty BaseRepository>]>::Record,
                        <Self as [<#relation_ty BaseRepository>]>::Id,
                    )>
                } } },
            };

            quote! {
                repositories_paste! {
                    fn #load_by_multiple_fn_name(
                        #relation_plural: &Vec<&<Self as [<#relation_ty BaseRepository>]>::Record>,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    where
                        Self: [<#relation_ty ReadRepository>]
                    #body
                }
            }
        }

        pub (crate) fn get_by_single(relation: &RelationInput, repository: &RepositoryInput, relation_repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let relation_load_in_singles: Vec<_> = repository.relations
                .iter()
                .map(|relation| shared::load_in_single(repository, relation))
                .collect();
            let relation_load_in_multiples: Vec<_> = repository.relations
                .iter()
                .map(|relation| load_in_multiple(repository, relation))
                .collect();

            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let get_by_single_fn_name = op_by_single_fn_name("get", relation, relation_repository);
            let load_by_multiple_fn_name = op_by_multiple_fn_name("load", relation, relation_repository);

            let return_ty = match relation.cardinality {
                Cardinality::One => { quote! { repositories_paste! {
                    Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>
                } } },
                Cardinality::OneOrNone => { quote! { repositories_paste! {
                    Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>
                } } },
                Cardinality::Many|Cardinality::AtLeastOne => { quote! { repositories_paste! {
                    Vec<(
                        Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>,
                        <Self as [<#ty BaseRepository>]>::Id,
                        <Self as [<#relation_ty BaseRepository>]>::Id,
                    )>
                } } },
            };

            let body = match relation.cardinality {
                Cardinality::One => quote! { repositories_paste! {
                    let adaptor_record = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?.pop().unwrap();

                    let (record, #singular) = self.[<foo_store_ #singular>](adaptor_record);
                    let #singular = #singular.deref();

                    let mut loaded_relations = [<Loaded #ty Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_singles
                        }
                    )*

                    Ok(Entity {
                        value: record,
                        relations: loaded_relations,
                    })
                } },
                Cardinality::OneOrNone => quote! { repositories_paste! {
                    let adaptor_record_option = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?.pop().unwrap();

                    let adaptor_record = match adaptor_record_option {
                        Some(adaptor_record) => adaptor_record,
                        None => return Ok(None),
                    };

                    let (record, #singular) = self.[<foo_store_ #singular>](adaptor_record);
                    let #singular = #singular.deref();

                    let mut loaded_relations = [<Loaded #ty Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_singles
                        }
                    )*

                    Ok(Some(Entity {
                        value: record,
                        relations: loaded_relations,
                    }))
                } },
                Cardinality::Many => quote! { repositories_paste! {
                    let [<#relation_singular _id>] = #relation_singular.as_ref().clone();

                    let adaptor_records: Vec<_> = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?
                        .into_iter()
                        .map(|(adaptor_record, _)| adaptor_record)
                        .collect();

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        repositories_izip!(records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(record, loaded_relations)| {
                                let record_id: <Self as [<#ty BaseRepository>]>::Id = record.deref().as_ref().clone().into();
                                (
                                    Entity {
                                        value: record,
                                        relations: loaded_relations,
                                    },
                                    record_id,
                                    [<#relation_singular _id>].clone(),
                                )
                            })
                            .collect()
                    )
                } },
                Cardinality::AtLeastOne => quote! { repositories_paste! {
                    let [<#relation_singular _id>] = #relation_singular.as_ref().clone();

                    let adaptor_records: Vec<_> = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?
                        .into_iter()
                        .map(|(adaptor_record, _)| adaptor_record)
                        .collect();

                    if adaptor_records.len() == 0 {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        repositories_izip!(records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(record, loaded_relations)| {
                                let record_id: <Self as [<#ty BaseRepository>]>::Id = record.deref().as_ref().clone().into();
                                (
                                    Entity {
                                        value: record,
                                        relations: loaded_relations,
                                    },
                                    record_id,
                                    [<#relation_singular _id>].clone(),
                                )
                            })
                            .collect()
                    )
                } },
            };

            quote! {
                repositories_paste! {
                    fn #get_by_single_fn_name(&mut self, #relation_singular: &<Self as [<#relation_ty BaseRepository>]>::Record, load_relations: &[<Load #ty Relations>], client: Self::Client<'_>) -> Result<#return_ty, Self::Error>
                    where
                        Self: #read_repositories
                    {
                        #body
                    }
                }
            }
        }

        pub (crate) fn get_by_multiple(relation: &RelationInput, repository: &RepositoryInput, relation_repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let relation_load_in_multiples: Vec<_> = repository.relations
                .iter()
                .map(|relation| load_in_multiple(repository, relation))
                .collect();

            let relation_ty = relation_repository.ty();
            let relation_plural = relation_repository.plural();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            let get_by_multiple_fn_name = op_by_multiple_fn_name("get", relation, relation_repository);
            let load_by_multiple_fn_name = op_by_multiple_fn_name("load", relation, relation_repository);

            let return_ty = match relation.cardinality {
                Cardinality::One => { quote! { repositories_paste! {
                    Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>
                } } },
                Cardinality::OneOrNone => { quote! { repositories_paste! {
                    Vec<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>
                } } },
                Cardinality::Many|Cardinality::AtLeastOne => { quote! { repositories_paste! {
                    Vec<(
                        Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>,
                        <Self as [<#ty BaseRepository>]>::Id,
                        <Self as [<#relation_ty BaseRepository>]>::Id,
                    )>
                } } },
            };

            let body = match relation.cardinality {
                Cardinality::One => quote! { repositories_paste! {
                    let num_requested_records = #relation_plural.len();
                    let adaptor_records = Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    if adaptor_records.len() != num_requested_records {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        repositories_izip!(records.into_iter(), all_loaded_relations.into_iter())
                            .map(|(record, loaded_relations)| Entity {
                                value: record,
                                relations: loaded_relations,
                            })
                            .collect()
                    )
                } },
                Cardinality::OneOrNone => quote! { repositories_paste! {
                    let num_requested_records = #relation_plural.len();
                    let adaptor_record_options =Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    if adaptor_record_options.len() != num_requested_records {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (adaptor_records, indices_of_existing_records) = repositories_transpose_2::<<Self as [<#ty BaseRepository>]>::Record, usize>(
                        adaptor_record_options
                            .into_iter()
                            .enumerate()
                            .filter_map(|(i, adaptor_record_option)| adaptor_record_option.map(|adaptor_record| (adaptor_record, i)))
                            .collect()
                    );

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..records.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    let mut entity_options: Vec<Option<Entity<
                        #sync_ptr<#ty>,
                        [<Loaded #ty Relations>],
                    >>> = Vec::with_capacity(num_requested_records);

                    let mut option_index = 0;
                    for (i, (record, loaded_relations)) in repositories_izip!(records.into_iter(), all_loaded_relations.into_iter()).enumerate() {
                        let desired_option_index = indices_of_existing_records[i];
                        while option_index < desired_option_index {
                            entity_options.push(None);
                            option_index += 1;
                        }
                        entity_options.push(Some(Entity {
                            value: record,
                            relations: loaded_relations,
                        }));
                    }

                    Ok(entity_options)
                } },
                Cardinality::Many => quote! { repositories_paste! {
                    let adaptor_records_and_parent_ids = Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    let (adaptor_records, parent_ids) = repositories_transpose_2(adaptor_records_and_parent_ids);

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let records_and_parent_ids: Vec<_> = repositories_izip!(records.into_iter(), parent_ids.into_iter()).collect();

                    let unique_record_ids: Vec<<Self as [<#ty BaseRepository>]>::Id> = records_and_parent_ids
                        .iter()
                        .map(|(record, _)| record.deref().as_ref())
                        .unique()
                        .map(|record_id_ref| record_id_ref.clone().into())
                        .collect();

                    let mut all_loaded_relations: Vec<_> = (0..unique_record_ids.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    let all_loaded_relations: std::collections::HashMap<_, _> = repositories_izip!(
                        unique_record_ids.into_iter(),
                        all_loaded_relations.into_iter(),
                    ).collect();

                    Ok(
                        records_and_parent_ids
                            .into_iter()
                            .map(|(record, parent_id)| {
                                let record_id: <Self as [<#ty BaseRepository>]>::Id = record.deref().as_ref().clone().into();
                                (
                                    Entity {
                                        value: record,
                                        relations: all_loaded_relations[&record_id].clone(),
                                    },
                                    record_id,
                                    parent_id,
                                )
                            })
                            .collect()
                    )
                } },
                Cardinality::AtLeastOne => quote! { repositories_paste! {
                    let adaptor_records_and_parent_ids = Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    let unique_parent_ids_found: Vec<_> = adaptor_records_and_parent_ids
                        .iter()
                        .map(|(_, parent_id)| parent_id)
                        .unique()
                        .collect();

                    if unique_parent_ids_found.len() != #relation_plural.len() {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (adaptor_records, parent_ids) = repositories_transpose_2(adaptor_records_and_parent_ids);

                    let (records, [<#plural _ptrs>]) = self.[<store_ #plural>](adaptor_records);
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|ptr| ptr.deref()).collect();

                    let records_and_parent_ids: Vec<_> = repositories_izip!(records.into_iter(), parent_ids.into_iter()).collect();

                    let unique_record_ids: Vec<<Self as [<#ty BaseRepository>]>::Id> = records_and_parent_ids
                        .iter()
                        .map(|(record, _)| record.deref().as_ref())
                        .unique()
                        .map(|record_id_ref| record_id_ref.clone().into())
                        .collect();

                    let mut all_loaded_relations: Vec<_> = (0..unique_record_ids.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.#relation_snakes.as_ref() {
                            #relation_load_in_multiples
                        }
                    )*

                    let all_loaded_relations: std::collections::HashMap<_, _> = repositories_izip!(
                        unique_record_ids.into_iter(),
                        all_loaded_relations.into_iter(),
                    ).collect();

                    Ok(
                        records_and_parent_ids
                            .into_iter()
                            .map(|(record, parent_id)| {
                                let record_id: <Self as [<#ty BaseRepository>]>::Id = record.deref().as_ref().clone().into();
                                (
                                    Entity {
                                        value: record,
                                        relations: all_loaded_relations[&record_id].clone(),
                                    },
                                    record_id,
                                    parent_id,
                                )
                            })
                            .collect()
                    )
                } },
            };

            quote! {
                repositories_paste! {
                    fn #get_by_multiple_fn_name(&mut self, #relation_plural: &Vec<&<Self as [<#relation_ty BaseRepository>]>::Record>, load_relations: &[<Load #ty Relations>], client: Self::Client<'_>) -> Result<#return_ty, Self::Error>
                    where
                        Self: #read_repositories
                    {
                        #body
                    }
                }
            }
        }

        pub (crate) fn load_by_field_multiple(repository: &RepositoryInput, load_by: &LoadByInput, todo: bool) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();

            let load_by_ty = load_by.ty();
            let load_by_plural = load_by.plural();

            let load_by_field_multiple_fn_name = format_ident!("load_{}_by_{}_{}", plural, singular, load_by_plural);

            let body = if todo { quote! { { todo!() } } } else { quote! { ; } };

            let return_ty = match load_by.cardinality {
                Cardinality::One|Cardinality::Many|Cardinality::AtLeastOne => { quote! { repositories_paste! {
                    Vec<<Self as [<#ty BaseRepository>]>::Record>
                } } },
                Cardinality::OneOrNone => { quote! { repositories_paste! {
                    Vec<Option<<Self as [<#ty BaseRepository>]>::Record>>
                } } },
            };

            quote! {
                repositories_paste! {
                    fn #load_by_field_multiple_fn_name(
                        #load_by_plural: Vec<#load_by_ty>,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    #body
                }
            }
        }

        pub (crate) fn get_by_field(repository: &RepositoryInput, load_by: &LoadByInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();

            let load_by_ty = load_by.ty();
            let load_by_singular = load_by.singular();
            let load_by_plural = load_by.plural();

            let load_by_field_multiple_fn_name = format_ident!("load_{}_by_{}_{}", plural, singular, load_by_plural);

            match load_by.cardinality {
                Cardinality::One => quote! { repositories_paste! {
                    impl #ty {
                        pub fn [<get_by_ #load_by_singular>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(#load_by_singular: #load_by_ty) -> [<Get #ty Builder>]<Adaptor> {
                            [<Get #ty Builder>] {
                                adaptor: Adaptor::default(),
                                load_adaptor_record: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(vec![#load_by_singular], client)?
                                        .pop()
                                        .ok_or_else(|| <Adaptor as BaseRepository>::Error::not_found())?
                                )),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn [<get_by_ #load_by_plural>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(#load_by_plural: Vec<#load_by_ty>) -> [<Get #ty sBuilder>]<Adaptor> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: #load_by_plural.len() as isize,
                                load_adaptor_records: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(#load_by_plural, client)?
                                )),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                } },
                Cardinality::Many => quote! { repositories_paste! {
                    impl #ty {
                        pub fn [<get_by_ #load_by_singular>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(#load_by_singular: #load_by_ty) -> [<Get #ty sBuilder>]<Adaptor> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: -1,
                                load_adaptor_records: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(vec![#load_by_singular], client)?
                                )),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn [<get_by_ #load_by_plural>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(#load_by_plural: Vec<#load_by_ty>) -> [<Get #ty sBuilder>]<Adaptor> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: -1,
                                load_adaptor_records: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(#load_by_plural, client)?
                                )),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                } },
                _ => unreachable!(),
            }
        }
    }

    pub (crate) mod builders {
        use super::*;

        pub (crate) fn get_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Get #ty Builder>]<Adaptor: #read_repositories> {
                        adaptor: Adaptor,
                        load_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<<Adaptor as [<#ty BaseRepository>]>::Record, <Adaptor as BaseRepository>::Error>>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: #read_repositories> [<Get #ty Builder>]<Adaptor> {
                        #(
                            pub fn [<load_ #relation_snakes>](mut self) -> [<Get #ty Builder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes>]();
                                self
                            }

                            pub fn [<load_ #relation_snakes _with>](mut self, with_fn: impl FnOnce([<Load #relation_tys Relations>]) -> [<Load #relation_tys Relations>]) -> [<Get #ty Builder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes _with>](with_fn);
                                self
                            }
                        )*
                    }

                    impl<Adaptor: #read_repositories> [<Get #ty Builder>]<Adaptor>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            let adaptor_record = (self.load_adaptor_record)(client)?;
                            self.adaptor.[<get_ #singular>](adaptor_record, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn get<Adaptor: #read_repositories>(id: <Adaptor as [<#ty BaseRepository>]>::Id) -> [<Get #ty Builder>]<Adaptor> {
                            [<Get #ty Builder>] {
                                adaptor: Adaptor::default(),
                                load_adaptor_record: Box::new(move |client| Ok(Adaptor::[<load_ #plural>](vec![id], client)?.pop().unwrap())),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn load_into<Adaptor: #read_repositories>(
                            get_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<<Adaptor as [<#ty BaseRepository>]>::Record, <Adaptor as BaseRepository>::Error>>,
                        ) -> [<Get #ty Builder>]<Adaptor> {
                            [<Get #ty Builder>] {
                                adaptor: Adaptor::default(),
                                load_adaptor_record: get_adaptor_record,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn get_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Get #ty sBuilder>]<Adaptor: #read_repositories> {
                        adaptor: Adaptor,
                        num_requested_records: isize,
                        load_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: #read_repositories> [<Get #ty sBuilder>]<Adaptor> {
                        #(
                            pub fn [<load_ #relation_snakes>](mut self) -> [<Get #ty sBuilder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes>]();
                                self
                            }

                            pub fn [<load_ #relation_snakes _with>](mut self, with_fn: impl FnOnce([<Load #relation_tys Relations>]) -> [<Load #relation_tys Relations>]) -> [<Get #ty sBuilder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes _with>](with_fn);
                                self
                            }
                        )*
                    }

                    impl<Adaptor: #read_repositories> [<Get #ty sBuilder>]<Adaptor>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            let adaptor_records = (self.load_adaptor_records)(client)?;
                            if self.num_requested_records >= 0 && adaptor_records.len() as isize != self.num_requested_records {
                                return Err(<<Adaptor as BaseRepository>::Error as RepositoryError>::not_found());
                            }
                            self.adaptor.[<get_ #plural>](adaptor_records, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn get_batch<Adaptor: #read_repositories>(ids: Vec<<Adaptor as [<#ty BaseRepository>]>::Id>) -> [<Get #ty sBuilder>]<Adaptor> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: ids.len() as isize,
                                load_adaptor_records: Box::new(move |client| Adaptor::[<load_ #plural>](ids, client)),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn load_into_batch<Adaptor: #read_repositories>(
                            num_requested_records: usize,
                            get_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        ) -> [<Get #ty sBuilder>]<Adaptor> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: num_requested_records as isize,
                                load_adaptor_records: get_adaptor_records,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn try_get_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<TryGet #ty Builder>]<Adaptor: #read_repositories> {
                        adaptor: Adaptor,
                        try_load_adaptor_record: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Option<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: #read_repositories> [<TryGet #ty Builder>]<Adaptor> {
                        #(
                            pub fn [<load_ #relation_snakes>](mut self) -> [<TryGet #ty Builder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes>]();
                                self
                            }

                            pub fn [<load_ #relation_snakes _with>](mut self, with_fn: impl FnOnce([<Load #relation_tys Relations>]) -> [<Load #relation_tys Relations>]) -> [<TryGet #ty Builder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes _with>](with_fn);
                                self
                            }
                        )*
                    }

                    impl<Adaptor: #read_repositories> [<TryGet #ty Builder>]<Adaptor>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            let adaptor_record_option = (self.try_load_adaptor_record)(client)?;
                            self.adaptor.[<try_get_ #singular>](adaptor_record_option, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn try_get<Adaptor: #read_repositories>(id: <Adaptor as [<#ty BaseRepository>]>::Id) -> [<TryGet #ty Builder>]<Adaptor> {
                            [<TryGet #ty Builder>] {
                                adaptor: Adaptor::default(),
                                try_load_adaptor_record: Box::new(move |client| Ok(Adaptor::[<try_load_ #plural>](vec![id], client)?.pop().unwrap())),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn load_into_option<Adaptor: #read_repositories>(
                            get_adaptor_record_option: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Option<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        ) -> [<TryGet #ty Builder>]<Adaptor> {
                            [<TryGet #ty Builder>] {
                                adaptor: Adaptor::default(),
                                try_load_adaptor_record: get_adaptor_record_option,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn try_get_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let relation_snakes = repository.relation_snakes();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<TryGet #ty sBuilder>]<Adaptor: #read_repositories> {
                        adaptor: Adaptor,
                        num_requested_records: isize,
                        try_load_adaptor_records: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<<Adaptor as [<#ty BaseRepository>]>::Record>>, <Adaptor as BaseRepository>::Error>>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: #read_repositories> [<TryGet #ty sBuilder>]<Adaptor> {
                        #(
                            pub fn [<load_ #relation_snakes>](mut self) -> [<TryGet #ty sBuilder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes>]();
                                self
                            }

                            pub fn [<load_ #relation_snakes _with>](mut self, with_fn: impl FnOnce([<Load #relation_tys Relations>]) -> [<Load #relation_tys Relations>]) -> [<TryGet #ty sBuilder>]<Adaptor> {
                                self.load_relations = self.load_relations.[<load_ #relation_snakes _with>](with_fn);
                                self
                            }
                        )*
                    }

                    impl<Adaptor: #read_repositories> [<TryGet #ty sBuilder>]<Adaptor>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            let adaptor_record_options = (self.try_load_adaptor_records)(client)?;
                            if self.num_requested_records >= 0 && adaptor_record_options.len() as isize != self.num_requested_records {
                                return Err(<<Adaptor as BaseRepository>::Error as RepositoryError>::not_found());
                            }
                            self.adaptor.[<try_get_ #plural>](adaptor_record_options, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn try_get_batch<Adaptor: #read_repositories>(ids: Vec<<Adaptor as [<#ty BaseRepository>]>::Id>) -> [<TryGet #ty sBuilder>]<Adaptor> {
                            [<TryGet #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: ids.len() as isize,
                                try_load_adaptor_records: Box::new(move |client| Adaptor::[<try_load_ #plural>](ids, client)),
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }

                        pub fn load_into_option_batch<Adaptor: #read_repositories>(
                            num_requested_records: usize,
                            get_adaptor_record_options: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<<Adaptor as [<#ty BaseRepository>]>::Record>>, <Adaptor as BaseRepository>::Error>>,
                        ) -> [<TryGet #ty sBuilder>]<Adaptor> {
                            [<TryGet #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                num_requested_records: num_requested_records as isize,
                                try_load_adaptor_records: get_adaptor_record_options,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }
    }
}

pub (crate) mod write_repositories {
    use super::*;

    pub (crate) fn write_repositories(input: &RepositoriesInput) -> TokenStream2 {
        let write_repositories: Vec<_> = input.repositories
            .iter()
            .filter(|repository| if let Mutability::RW = repository.mutability { true } else { false })
            .map(|repository| {
                let ty_write_repository = ty_write_repository::ty_write_repository(repository);
                let delete_ty_singular_builder = builders::delete_ty_singular_builder(repository);
                let delete_ty_multiple_builder = builders::delete_ty_multiple_builder(repository);
                let insert_ty_singular_builder = builders::insert_ty_singular_builder(repository);
                let insert_ty_multiple_builder = builders::insert_ty_multiple_builder(repository);
                let update_ty_singular_builder = builders::update_ty_singular_builder(repository);
                let update_ty_multiple_builder = builders::update_ty_multiple_builder(repository);

                quote! {
                    #ty_write_repository
                    #delete_ty_singular_builder
                    #delete_ty_multiple_builder
                    #insert_ty_singular_builder
                    #insert_ty_multiple_builder
                    #update_ty_singular_builder
                    #update_ty_multiple_builder
                }
            })
            .collect();

        quote! {
            #(#write_repositories)*
        }
    }

    pub (crate) mod ty_write_repository {
        use super::*;

        pub (crate) fn ty_write_repository(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();

            quote! {
                repositories_paste! {
                    pub trait [<#ty WriteRepository>]: [<#ty ReadRepository>] {
                        type RecordPost: From<[<#ty Post>]>;
                        type RecordPatch: From<[<#ty Patch>]>;

                        fn [<delete_ #plural>](
                            [<#singular _ids>]: Vec<<Self as [<#ty BaseRepository>]>::Id>,
                            client: Self::Client<'_>,
                        ) -> Result<usize, Self::Error>;

                        fn [<insert_ #plural>](
                            [<#singular _posts>]: Vec<<Self as [<#ty WriteRepository>]>::RecordPost>,
                            client: Self::Client<'_>,
                        ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error>;

                        fn [<update_ #plural>](
                            [<#singular _patches>]: Vec<<Self as [<#ty WriteRepository>]>::RecordPatch>,
                            client: Self::Client<'_>,
                        ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error>;
                    }
                }
            }
        }
    }

    pub (crate) mod builders {
        use super::*;

        pub (crate) fn delete_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();

            quote! {
                repositories_paste! {
                    pub struct [<Delete #ty Builder>]<Adaptor: [<#ty WriteRepository>]> {
                        id: <Adaptor as [<#ty BaseRepository>]>::Id,
                    }

                    impl<Adaptor: [<#ty WriteRepository>]> [<Delete #ty Builder>]<Adaptor> {
                        pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                            let _write_lock = Adaptor::write()?;
                            Adaptor::[<delete_ #plural>](vec![self.id], client)
                        }
                    }

                    impl #ty {
                        pub fn delete<Adaptor: [<#ty WriteRepository>]>(id: <Adaptor as [<#ty BaseRepository>]>::Id) -> [<Delete #ty Builder>]<Adaptor> {
                            [<Delete #ty Builder>] {
                                id,
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn delete_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();

            quote! {
                repositories_paste! {
                    pub struct [<Delete #ty sBuilder>]<Adaptor: [<#ty WriteRepository>]> {
                        ids: Vec<<Adaptor as [<#ty BaseRepository>]>::Id>,
                    }

                    impl<Adaptor: [<#ty WriteRepository>]> [<Delete #ty sBuilder>]<Adaptor> {
                        pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                            let _write_lock = Adaptor::write()?;
                            Adaptor::[<delete_ #plural>](self.ids, client)
                        }
                    }

                    impl #ty {
                        pub fn delete_batch<Adaptor: [<#ty WriteRepository>]>(ids: Vec<<Adaptor as [<#ty BaseRepository>]>::Id>) -> [<Delete #ty sBuilder>]<Adaptor> {
                            [<Delete #ty sBuilder>] {
                                ids,
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn insert_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Insert #ty Builder>]<Adaptor: [<#ty WriteRepository>] + #read_repositories> {
                        adaptor: Adaptor,
                        post: [<#ty Post>],
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories> [<Insert #ty Builder>]<Adaptor> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_record = {
                                let _write_lock = Adaptor::write()?;
                                let adaptor_post = <Adaptor as [<#ty WriteRepository>]>::RecordPost::from(self.post);
                                Adaptor::[<insert_ #plural>](vec![adaptor_post], client)?.pop().unwrap()
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #singular>](adaptor_record, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn insert<Adaptor: [<#ty WriteRepository>] + #read_repositories>(post: [<#ty Post>]) -> [<Insert #ty Builder>]<Adaptor> {
                            [<Insert #ty Builder>] {
                                adaptor: Adaptor::default(),
                                post,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn insert_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Insert #ty sBuilder>]<Adaptor: [<#ty WriteRepository>] + #read_repositories> {
                        adaptor: Adaptor,
                        posts: Vec<[<#ty Post>]>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories> [<Insert #ty sBuilder>]<Adaptor> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_records = {
                                let _write_lock = Adaptor::write()?;
                                let adaptor_posts = self.posts.into_iter().map(<Adaptor as [<#ty WriteRepository>]>::RecordPost::from).collect();
                                Adaptor::[<insert_ #plural>](adaptor_posts, client)?
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](adaptor_records, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn insert_batch<Adaptor: [<#ty WriteRepository>] + #read_repositories>(posts: Vec<[<#ty Post>]>) -> [<Insert #ty sBuilder>]<Adaptor> {
                            [<Insert #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                posts,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn update_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Update #ty Builder>]<Adaptor: [<#ty WriteRepository>] + #read_repositories> {
                        adaptor: Adaptor,
                        patch: [<#ty Patch>],
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories> [<Update #ty Builder>]<Adaptor> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_record = {
                                let _write_lock = Adaptor::write()?;
                                let adaptor_patch = <Adaptor as [<#ty WriteRepository>]>::RecordPatch::from(self.patch);
                                Adaptor::[<update_ #plural>](vec![adaptor_patch], client)?.pop().unwrap()
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #singular>](adaptor_record, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn update<Adaptor: [<#ty WriteRepository>] + #read_repositories>(patch: [<#ty Patch>]) -> [<Update #ty Builder>]<Adaptor> {
                            [<Update #ty Builder>] {
                                adaptor: Adaptor::default(),
                                patch,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub (crate) fn update_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let sync_ptr = repository.sync_ptr();
            let read_repositories = repository.read_repositories();

            quote! {
                repositories_paste! {
                    pub struct [<Update #ty sBuilder>]<Adaptor: [<#ty WriteRepository>] + #read_repositories> {
                        adaptor: Adaptor,
                        patches: Vec<[<#ty Patch>]>,
                        load_relations: [<Load #ty Relations>],
                    }

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories> [<Update #ty sBuilder>]<Adaptor> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_records = {
                                let _write_lock = Adaptor::write()?;
                                let adaptor_patches = self.patches.into_iter().map(<Adaptor as [<#ty WriteRepository>]>::RecordPatch::from).collect();
                                Adaptor::[<update_ #plural>](adaptor_patches, client)?
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](adaptor_records, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn update_batch<Adaptor: [<#ty WriteRepository>] + #read_repositories>(patches: Vec<[<#ty Patch>]>) -> [<Update #ty sBuilder>]<Adaptor> {
                            [<Update #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                patches,
                                load_relations: [<Load #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }
    }
}

pub (crate) mod shared {
    use super::*;

    /*
        {
            Road: {
                city: (City, One),
            },
            City: {
                avenues: (Road, Many),
            }
        }

        op_by_single_fn_name(
            "load",
            RelationInput { city: (City, One) }
            RepositoryInput { City },
            RepositoryInput { Road },
        ) == "load_road_cities_by_road"

        op_by_single_fn_name(
            "load",
            RelationInput { avenues: (Road, Many) }
            RepositoryInput { Road },
            RepositoryInput { City },
        ) == "load_avenues_by_city"
    */
    pub (crate) fn op_by_single_fn_name(op: &str, relation: &RelationInput, relation_repository: &RepositoryInput) -> Ident {
        let relation_ty_singular = relation_repository.singular(); // road | city
        let relation_plural = relation.plural(); // cities | avenues
        format_ident!("{}_{}_{}_by_{}", op, relation_ty_singular, relation_plural, relation_ty_singular)
    }

    /*
        op_by_multiple_fn_name(
            "load",
            RelationInput { city: (City, One) }
            RepositoryInput { City },
            RepositoryInput { Road },
        ) == "load_road_cities_by_roads"

        op_by_multiple_fn_name(
            "load",
            RelationInput { avenues: (Road, Many) }
            RepositoryInput { Road },
            RepositoryInput { City },
        ) == "load_avenues_by_cities"
    */
    pub (crate) fn op_by_multiple_fn_name(op: &str, relation: &RelationInput, relation_repository: &RepositoryInput) -> Ident {
        let relation_plural = relation.plural(); // cities | avenues
        let relation_ty_plural = relation_repository.plural(); // roads | cities
        format_ident!("{}_{}_by_{}", op, relation_plural, relation_ty_plural)
    }

    pub (crate) fn load_in_single(repository: &RepositoryInput, relation: &RelationInput) -> TokenStream2 {
        let singular = repository.singular();
        let relation_snake = relation.snake();
        let get_by_single_fn_name = op_by_single_fn_name("get", relation, repository);

        match relation.cardinality {
            Cardinality::One => quote! {
                repositories_paste! {
                    let related_entity = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    loaded_relations.#relation_snake = Some(Box::new(related_entity));
                }
            },
            Cardinality::OneOrNone => quote! {
                repositories_paste! {
                    let related_entity_option = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    loaded_relations.#relation_snake = Some(Box::new(related_entity_option));
                }
            },
            Cardinality::Many => quote! {
                repositories_paste! {
                    let related_entities_and_ids = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    let related_entities: Vec<_> = related_entities_and_ids.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                    loaded_relations.#relation_snake = Some(Box::new(related_entities));
                }
            },
            Cardinality::AtLeastOne => quote! {
                repositories_paste! {
                    let related_entities_and_ids = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    let related_entities: Vec<_> = related_entities_and_ids.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                    if related_entities.len() == 0 {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }
                    loaded_relations.#relation_snake = Some(Box::new(related_entities));
                }
            },
        }
    }

    pub (crate) fn load_in_multiple(repository: &RepositoryInput, relation: &RelationInput) -> TokenStream2 {
        let ty = repository.ty();
        let plural = repository.plural();
        let relation_snake = relation.snake();
        let get_by_multiple_fn_name = op_by_multiple_fn_name("get", relation, repository);
        let relation_ty = relation.ty();
        let sync_ptr = relation.sync_ptr();

        match relation.cardinality {
            Cardinality::One => quote! {
                repositories_paste! {
                    let related_entities = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    for (loaded_relations, related_entity) in repositories_izip!(all_loaded_relations.iter_mut(), related_entities.into_iter()) {
                        loaded_relations.#relation_snake = Some(Box::new(related_entity));
                    }
                }
            },
            Cardinality::OneOrNone => quote! {
                repositories_paste! {
                    let related_entity_options = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    for (loaded_relations, related_entity_option) in repositories_izip!(all_loaded_relations.iter_mut(), related_entity_options.into_iter()) {
                        loaded_relations.#relation_snake = Some(Box::new(related_entity_option));
                    }
                }
            },
            Cardinality::Many => quote! {
                repositories_paste! {
                    let all_related_entities_and_ids = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    let mut all_related_entities_by_parent_ids: std::collections::HashMap<
                        <Self as [<#ty BaseRepository>]>::Id,
                        std::collections::HashMap<
                            <Self as [<#relation_ty BaseRepository>]>::Id,
                            Vec<Entity<#sync_ptr<#relation_ty>, [<Loaded #relation_ty Relations>]>>,
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
                        loaded_relations.#relation_snake = Some(Box::new(match all_related_entities_by_parent_ids.get_mut(&#plural[i].as_ref()) {
                            Some(all_related_entities_by_related_ids) => {
                                all_related_entities_by_related_ids
                                    .values_mut()
                                    .map(|related_entity_dupes| related_entity_dupes.pop().unwrap())
                                    .collect()
                            },
                            None => vec![],
                        }));
                    }
                }
            },
            Cardinality::AtLeastOne => quote! {
                repositories_paste! {
                    let all_related_entities_and_ids = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    let mut all_related_entities_by_parent_ids: std::collections::HashMap<
                        <Self as [<#ty BaseRepository>]>::Id,
                        std::collections::HashMap<
                            <Self as [<#relation_ty BaseRepository>]>::Id,
                            Vec<Entity<#sync_ptr<#relation_ty>, [<Loaded #relation_ty Relations>]>>,
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
                        loaded_relations.#relation_snake = Some(Box::new(match all_related_entities_by_parent_ids.get_mut(&#plural[i].as_ref()) {
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
                }
            },
        }
    }
}
