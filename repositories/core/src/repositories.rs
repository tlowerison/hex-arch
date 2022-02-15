use crate::input::*;
use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use syn::Ident;

pub fn repositories(input: RepositoriesInput, todo: bool) -> TokenStream2 {
    let read_repositories = read_repositories::read_repositories(&input, todo);
    let write_repositories = write_repositories::write_repositories(&input, todo);

    let deref = quote! { use std::ops::Deref as RepositoriesDeref; };

    let readables: Vec<_> = input
        .repositories
        .iter()
        .map(|repository| repository.ty())
        .collect();
    let writables: Vec<_> = input
        .repositories
        .iter()
        .filter_map(|repository| match repository.mutability {
            Mutability::RW => Some(repository.ty()),
            Mutability::R => None,
        })
        .collect();

    let namespaced: TokenStream2 = input.name
        .as_ref()
        .map(|name| quote! {
            hex_arch_paste! {
                pub trait [<#name ReadRepository>] = #([<#readables ReadRepository>])+*;
                pub trait [<#name ReadWriteRepository>] = [<#name ReadRepository>] + #([<#writables WriteRepository>])+*;
            }
        })
        .unwrap_or(quote! {});

    let tokens = if todo {
        quote! {
            #read_repositories
            #write_repositories
        }
    } else {
        quote! {
            #deref
            #namespaced
            #read_repositories
            #write_repositories
        }
    };
    tokens
}

pub mod read_repositories {
    use super::*;

    pub fn read_repositories(input: &RepositoriesInput, todo: bool) -> TokenStream2 {
        let read_repositories: Vec<_> = input
            .repositories
            .iter()
            .map(|repository| {
                let ty_base_repository = ty_base_repository(repository);
                let ty_read_repository =
                    ty_read_repository::ty_read_repository(repository, input, todo);

                if todo {
                    quote! {
                        #ty_base_repository
                        #ty_read_repository
                    }
                } else {
                    let ty_entity = ty_entity(repository);
                    let load_ty_relations = load_ty_relations(repository, input);
                    let loaded_ty_relations = loaded_ty_relations(repository);
                    let get_ty_singular_builder =
                        builders::get_ty_singular_builder(repository, input);
                    let get_ty_multiple_builder =
                        builders::get_ty_multiple_builder(repository, input);
                    let try_get_ty_singular_builder =
                        builders::try_get_ty_singular_builder(repository, input);
                    let try_get_ty_multiple_builder =
                        builders::try_get_ty_multiple_builder(repository, input);
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
                }
            })
            .collect();

        let load_ty_relation_traits: Vec<_> = input
            .repositories
            .iter()
            .map(load_ty_relation_trait)
            .collect();

        let load_mod = &input.load_relations_trait_mod;

        quote! {
            hex_arch_paste! {
                pub mod #load_mod {
                    use super::*;

                    #(#load_ty_relation_traits)*
                }

                #(#read_repositories)*
            }
        }
    }

    fn ty_entity(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let sync_ptr = repository.sync_ptr();
        quote! {
            hex_arch_paste! {
                pub type [<#ty Entity>] = Entity<#sync_ptr<#ty>, [<Loaded #ty Relations>]>;
            }
        }
    }

    fn load_ty_relation_trait(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let relation_tys = repository.relation_tys();
        let relation_snakes = repository.relation_snakes();
        let relation_pascals = repository.relation_pascals();

        quote! {
            hex_arch_paste! {
                pub trait [<Load #ty RelationsTraitRef>] {
                    type Cursor: Clone;
                    type WhereExprRef<'a> where Self: 'a;
                    #(
                        type [<Load #relation_pascals RelationsRef>]: [<Load #relation_tys RelationsTraitRef>];
                    )*

                    fn should_load(&self) -> bool;
                    fn where_expr_ref(&self) -> Option<Self::WhereExprRef<'_>>;
                    fn paginate_ref(&self) -> Option<&Paginate<Self::Cursor>>;

                    #(
                        fn [<#relation_snakes _ref>](&self) -> Option<&Self::[<Load #relation_pascals RelationsRef>]>;
                    )*
                }

                pub trait [<Load #ty RelationsTrait>]: [<Load #ty RelationsTraitRef>] {

                    type WithWhere<NewWhereExpr>;
                    #(
                        type [<Load #relation_pascals Relations>]: [<Load #relation_tys RelationsTrait>];
                        type [<WithLoad #relation_pascals Relations>]<LR: [<Load #relation_tys RelationsTrait>]>;
                    )*

                    fn as_dyn(self) -> Option<[<DynLoad #ty Relations>]>;

                    fn r#where<NewWhereExpr>(self, where_expr: NewWhereExpr) -> Self::WithWhere<NewWhereExpr>;

                    fn paginate(self, paginate: Paginate<Self::Cursor>) -> Self;

                    #(
                        fn [<load_ #relation_snakes>](self) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]>;

                        fn [<load_ #relation_snakes _with>]<LR: [<Load #relation_tys RelationsTrait>]>(
                            self,
                            with_fn: impl FnOnce(Self::[<Load #relation_pascals Relations>]) -> LR,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<LR>;

                        fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            self,
                            where_expr: NewWhereExpr,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>>;
                    )*
                }
            }
        }
    }

    fn load_ty_relations(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
        let ty = repository.ty();
        let key_ty = repository.key_ty();
        let relation_tys = repository.relation_tys();
        let relation_snakes = repository.relation_snakes();
        let relation_pascals = repository.relation_pascals();
        let load_mod = &input.load_relations_trait_mod;

        let other_relation_snakes: Vec<Vec<_>> = relation_snakes
            .iter()
            .map(|relation_snake|
                relation_snakes
                    .iter()
                    .filter(|x| *x != relation_snake)
                    .collect()
            )
            .collect();

        let other_relation_tys: Vec<Vec<_>> = relation_snakes
            .iter()
            .map(|relation_snake|
                izip!(relation_snakes.iter(), relation_tys.iter())
                    .filter(|(x, _)| *x != relation_snake)
                    .map(|(_, relation_ty)| relation_ty)
                    .collect()
            )
            .collect();

        let replaced_sub_relations: Vec<Vec<_>> = relation_pascals
            .iter()
            .map(|relation_pascal|
                relation_pascals
                    .iter()
                    .map(|x| if x == relation_pascal { format_ident!("LR") } else { format_ident!("Load{}Relations", x) })
                    .collect()
            )
            .collect();

        let replaced_uninit_sub_relations: Vec<Vec<_>> = relation_pascals
            .iter()
            .map(|relation_pascal|
                izip!(relation_pascals.iter(), relation_tys.iter())
                    .map(|(x, relation_ty)| if x == relation_pascal { format_ident!("LR") } else { format_ident!("UninitLoad{}Relations", relation_ty) })
                    .collect()
            )
            .collect();

        quote! {
            hex_arch_paste! {
                #[derive(Clone, Debug, Default)]
                pub struct [<UninitLoad #ty Relations>];

                impl #load_mod::[<Load #ty RelationsTraitRef>] for [<UninitLoad #ty Relations>] {
                    type Cursor = ();
                    type WhereExprRef<'a> = &'a ();
                    #(
                        type [<Load #relation_pascals RelationsRef>] = [<UninitLoad #relation_tys Relations>];
                    )*

                    fn should_load(&self) -> bool { false }
                    fn where_expr_ref(&self) -> Option<Self::WhereExprRef<'_>> { None }
                    fn paginate_ref(&self) -> Option<&Paginate<Self::Cursor>> { None }
                    #(
                        fn [<#relation_snakes _ref>](&self) -> Option<&Self::[<Load #relation_pascals RelationsRef>]> { None }
                    )*
                }

                impl #load_mod::[<Load #ty RelationsTrait>] for [<UninitLoad #ty Relations>] {

                    type WithWhere<NewWhereExpr> = [<StaticLoad #ty Relations>]<NewWhereExpr>;
                    #(
                        type [<Load #relation_pascals Relations>] = [<StaticLoad #relation_tys Relations>];
                        type [<WithLoad #relation_pascals Relations>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]> = [<StaticLoad #ty Relations>]<
                            (),
                            #(#replaced_uninit_sub_relations,)*
                        >;
                    )*

                    fn as_dyn(self) -> Option<[<DynLoad #ty Relations>]> {
                        None
                    }

                    fn r#where<NewWhereExpr>(self, where_expr: NewWhereExpr) -> Self::WithWhere<NewWhereExpr> {
                        let relations: [<StaticLoad #ty Relations>] = Default::default();
                        relations.r#where(where_expr)
                    }

                    fn paginate(self, paginate: Paginate<Self::Cursor>) -> Self { self }

                    #(
                        fn [<load_ #relation_snakes>](self) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]> {
                            let relations: [<StaticLoad #ty Relations>] = Default::default();
                            relations.[<load_ #relation_snakes>]()
                        }

                        fn [<load_ #relation_snakes _with>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]>(
                            self,
                            with_fn: impl FnOnce(Self::[<Load #relation_pascals Relations>]) -> LR,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<LR> {
                            [<StaticLoad #ty Relations>] {
                                where_expr: None,
                                paginate: None,
                                #relation_snakes: with_fn(Default::default()),
                                #(#other_relation_snakes: [<UninitLoad #other_relation_tys Relations>]),*
                            }
                        }

                        fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            self,
                            where_expr: NewWhereExpr,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>> {
                            let relations: [<StaticLoad #ty Relations>] = Default::default();
                            relations.[<load_ #relation_snakes _where>](where_expr)
                        }
                    )*
                }

                #[derive(Clone, Debug, Default)]
                pub struct [<StaticLoad #ty Relations>]<WhereExpr = (), #([<Load #relation_pascals Relations>] = [<UninitLoad #relation_tys Relations>]),*> {
                    pub paginate: Option<Paginate<#key_ty>>,
                    pub where_expr: Option<WhereExpr>,
                    #(pub #relation_snakes: [<Load #relation_pascals Relations>],)*
                }

                impl [<StaticLoad #ty Relations>] {
                    fn with_where_expr<WhereExpr>(where_expr: WhereExpr) -> [<StaticLoad #ty Relations>]<WhereExpr> {
                        [<StaticLoad #ty Relations>] {
                            paginate: None,
                            where_expr: Some(where_expr),
                            #(#relation_snakes: Default::default(),)*
                        }
                    }
                }

                impl From<[<UninitLoad #ty Relations>]> for [<StaticLoad #ty Relations>] {
                    fn from(_: [<UninitLoad #ty Relations>]) -> [<StaticLoad #ty Relations>] {
                        Default::default()
                    }
                }

                impl From<()> for [<StaticLoad #ty Relations>] {
                    fn from(_: ()) -> [<StaticLoad #ty Relations>] {
                        Default::default()
                    }
                }

                impl<WhereExpr, #([<Load #relation_pascals Relations>]: #load_mod::[<Load #relation_tys RelationsTrait>]),*> [<StaticLoad #ty Relations>]<WhereExpr, #([<Load #relation_pascals Relations>]),*> {
                    pub fn as_static_dyn(self) -> [<StaticDynLoad #ty Relations>]<WhereExpr> {
                        [<StaticLoad #ty Relations>] {
                            paginate: self.paginate,
                            where_expr: self.where_expr,
                            #(#relation_snakes: self.#relation_snakes.as_dyn().unwrap(),)*
                        }
                    }
                }

                impl<
                    WhereExpr: 'static,
                    #([<Load #relation_pascals Relations>]: #load_mod::[<Load #relation_tys RelationsTraitRef>] + 'static),*
                > #load_mod::[<Load #ty RelationsTraitRef>] for [<StaticLoad #ty Relations>]<WhereExpr, #([<Load #relation_pascals Relations>]),*> {

                    type Cursor = #key_ty;
                    type WhereExprRef<'a> = &'a WhereExpr;
                    #(
                        type [<Load #relation_pascals RelationsRef>] = [<Load #relation_pascals Relations>];
                    )*

                    fn should_load(&self) -> bool { true }

                    fn where_expr_ref(&self) -> Option<Self::WhereExprRef<'_>> {
                        self.where_expr.as_ref()
                    }

                    fn paginate_ref(&self) -> Option<&Paginate<Self::Cursor>> {
                        self.paginate.as_ref()
                    }

                    #(
                        fn [<#relation_snakes _ref>](&self) -> Option<&Self::[<Load #relation_pascals RelationsRef>]> {
                            use #load_mod::*;

                            if self.#relation_snakes.should_load() {
                                Some(&self.#relation_snakes)
                            } else {
                                None
                            }
                        }
                    )*
                }

                impl<
                    WhereExpr: 'static,
                    #(
                        [<Load #relation_pascals Relations>]: #load_mod::[<Load #relation_tys RelationsTrait>] + 'static
                    ),*
                > #load_mod::[<Load #ty RelationsTrait>] for [<StaticLoad #ty Relations>]<WhereExpr, #([<Load #relation_pascals Relations>]),*> {

                    type WithWhere<NewWhereExpr> = [<StaticLoad #ty Relations>]<NewWhereExpr, #([<Load #relation_pascals Relations>]),*>;
                    #(
                        type [<Load #relation_pascals Relations>] = [<Load #relation_pascals Relations>];
                        type [<WithLoad #relation_pascals Relations>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]> = [<StaticLoad #ty Relations>]<
                            WhereExpr,
                            #(#replaced_sub_relations),*
                        >;
                    )*

                    fn as_dyn(self) -> Option<[<DynLoad #ty Relations>]> {
                        Some([<DynLoad #ty Relations>] {
                            paginate: self.paginate,
                            #(#relation_snakes: self.#relation_snakes.as_dyn().map(Box::new)),*
                        })
                    }

                    fn r#where<NewWhereExpr>(self, where_expr: NewWhereExpr) -> Self::WithWhere<NewWhereExpr> {
                        [<StaticLoad #ty Relations>] {
                            paginate: self.paginate,
                            where_expr: Some(where_expr),
                            #(#relation_snakes: self.#relation_snakes),*
                        }
                    }

                    fn paginate(mut self, paginate: Paginate<Self::Cursor>) -> Self {
                        self.paginate = Some(paginate);
                        self
                    }

                    #(
                        fn [<load_ #relation_snakes>](self) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]> {
                            [<StaticLoad #ty Relations>] {
                                paginate: self.paginate,
                                where_expr: self.where_expr,
                                #relation_snakes: Default::default(),
                                #(#other_relation_snakes: self.#other_relation_snakes),*
                            }
                        }

                        fn [<load_ #relation_snakes _with>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]>(
                            self,
                            with_fn: impl FnOnce(Self::[<Load #relation_pascals Relations>]) -> LR,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<LR> {
                            [<StaticLoad #ty Relations>] {
                                paginate: self.paginate,
                                where_expr: self.where_expr,
                                #relation_snakes: with_fn(self.#relation_snakes),
                                #(#other_relation_snakes: self.#other_relation_snakes,)*
                            }
                        }

                        fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            self,
                            where_expr: NewWhereExpr,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>> {
                            [<StaticLoad #ty Relations>] {
                                paginate: self.paginate,
                                where_expr: self.where_expr,
                                #relation_snakes: [<StaticLoad #relation_tys Relations>]::with_where_expr(where_expr),
                                #(#other_relation_snakes: self.#other_relation_snakes),*
                            }
                        }
                    )*
                }

                #[derive(Clone, Debug, Default)]
                pub struct [<DynLoad #ty Relations>] {
                    pub paginate: Option<Paginate<#key_ty>>,
                    #(pub #relation_snakes: Option<Box<[<DynLoad #relation_tys Relations>]>>),*
                }

                impl #load_mod::[<Load #ty RelationsTraitRef>] for [<DynLoad #ty Relations>] {

                    type Cursor = #key_ty;
                    type WhereExprRef<'a> = &'a ();
                    #(
                        type [<Load #relation_pascals RelationsRef>] = [<DynLoad #relation_tys Relations>];
                    )*

                    fn should_load(&self) -> bool { true }

                    fn where_expr_ref(&self) -> Option<Self::WhereExprRef<'_>> {
                        None
                    }

                    fn paginate_ref(&self) -> Option<&Paginate<Self::Cursor>> {
                        self.paginate.as_ref()
                    }

                    #(
                        fn [<#relation_snakes _ref>](&self) -> Option<&Self::[<Load #relation_pascals RelationsRef>]> {
                            self.#relation_snakes.as_ref().map(|x| x.deref())
                        }
                    )*
                }

                impl #load_mod::[<Load #ty RelationsTrait>] for [<DynLoad #ty Relations>] {

                    type WithWhere<NewWhereExpr> = [<DynLoad #ty Relations>];
                    #(
                        type [<Load #relation_pascals Relations>] = [<DynLoad #relation_tys Relations>];
                        type [<WithLoad #relation_pascals Relations>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]> = [<DynLoad #ty Relations>];
                    )*

                    fn as_dyn(self) -> Option<[<DynLoad #ty Relations>]> {
                        Some(self)
                    }

                    fn r#where<NewWhereExpr>(self, _: NewWhereExpr) -> Self::WithWhere<NewWhereExpr> {
                        self
                    }

                    fn paginate(mut self, paginate: Paginate<Self::Cursor>) -> Self {
                        self.paginate = Some(paginate);
                        self
                    }

                    #(
                        fn [<load_ #relation_snakes>](mut self) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]> {
                            self.#relation_snakes = Some(Box::new(Default::default()));
                            self
                        }

                        fn [<load_ #relation_snakes _with>]<LR: #load_mod::[<Load #relation_tys RelationsTrait>]>(
                            mut self,
                            with_fn: impl FnOnce(Self::[<Load #relation_pascals Relations>]) -> LR,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<LR> {
                            self.#relation_snakes = with_fn(if let Some(sub_relations) = self.#relation_snakes {
                                *sub_relations
                            } else {
                                Default::default()
                            }).as_dyn().map(Box::new);
                            self
                        }

                        fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            mut self,
                            _: NewWhereExpr,
                        ) -> Self::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>> {
                            self.#relation_snakes = Some(Box::new(Default::default()));
                            self
                        }
                    )*
                }

                pub type [<StaticDynLoad #ty Relations>]<WhereExpr = ()> = [<StaticLoad #ty Relations>]<WhereExpr, #([<DynLoad #relation_tys Relations>]),*>;
            }
        }
    }

    fn loaded_ty_relations(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let relation_snakes = repository.relation_snakes();
        let relation_types: Vec<_> = repository
            .relations
            .iter()
            .map(loaded_relation_type)
            .collect();

        quote! {
            hex_arch_paste! {
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

        match relation.cardinality {
            Cardinality::One => quote! {
                hex_arch_paste! {
                    Option<Box<[<#ty Entity>]>>
                }
            },
            Cardinality::OneOrNone => quote! {
                hex_arch_paste! {
                    Option<Box<Option<[<#ty Entity>]>>>
                }
            },
            Cardinality::AtLeastOne | Cardinality::Many => quote! {
                hex_arch_paste! {
                    Option<Box<Vec<[<#ty Entity>]>>>
                }
            },
        }
    }

    fn ty_base_repository(repository: &RepositoryInput) -> TokenStream2 {
        let ty = repository.ty();
        let key_type = &repository.key.ty;
        quote! {
            hex_arch_paste! {
                pub trait [<#ty BaseRepository>]: BaseRepository {
                    type Key: 'static + Sized + Clone + std::fmt::Debug + Eq + From<#key_type> + Into<#key_type> + PartialEq + std::hash::Hash;
                    type Record: 'static + Clone + std::fmt::Debug + Sized + AsRef<Self::Key> + Into<Self::Key> + Into<#ty>;
                }
            }
        }
    }

    pub mod ty_read_repository {
        use super::*;
        use shared::*;

        pub fn ty_read_repository(
            repository: &RepositoryInput,
            input: &RepositoriesInput,
            todo: bool,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let key_plural = repository.key_plural();

            let get_multiple = get_multiple(repository, input);

            let entity_storage = entity_storage(repository, todo);

            let inward_relations = repository.inward_relations(input);

            let body = if todo {
                quote! { { todo!() } }
            } else {
                quote! { ; }
            };

            let load_by_multiples: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| {
                    load_by_multiple(relation, repository, relation_repository, &body)
                })
                .collect();
            let load_by_multiple_keys: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| {
                    load_by_multiple_keys(relation, repository, relation_repository, &body)
                })
                .collect();

            let get_by_singles: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| {
                    get_by_single(relation, repository, relation_repository, input)
                })
                .collect();
            let get_by_multiples: Vec<_> = inward_relations
                .iter()
                .map(|(relation, relation_repository)| {
                    get_by_multiple(relation, repository, relation_repository, input)
                })
                .collect();

            let load_by_field_multiples: Vec<_> = repository
                .load_bys
                .iter()
                .map(|load_by| load_by_field_multiple(repository, load_by, &body, &body))
                .collect();

            let todo_fns = quote! {
                hex_arch_paste! {
                    #entity_storage

                    fn [<load_ #plural>](
                        [<#singular _ #key_plural>]: Vec<<Self as [<#ty BaseRepository>]>::Key>,
                        client: Self::Client<'_>,
                    ) -> Result<
                        PreSortValues<<Self as [<#ty BaseRepository>]>::Record>,
                        Self::Error,
                    >
                        #body

                    fn [<load_all_ #plural>](
                        client: Self::Client<'_>,
                    ) -> Result<
                        PreSortValues<<Self as [<#ty BaseRepository>]>::Record>,
                        Self::Error,
                    >
                        #body

                    #(
                        #load_by_multiples
                        #load_by_multiple_keys
                    )*

                    #(
                        #load_by_field_multiples
                    )*
                }
            };

            if todo {
                quote! {
                    hex_arch_paste! {
                        pub trait [<#ty ReadRepository>]: [<#ty BaseRepository>] {
                            #todo_fns
                        }
                    }
                }
            } else {
                quote! {
                    hex_arch_paste! {
                        pub trait [<#ty ReadRepository>]: [<#ty BaseRepository>] {
                            #todo_fns

                            #get_multiple
                            #(
                                #get_by_singles
                                #get_by_multiples
                            )*
                        }
                    }
                }
            }
        }

        fn entity_storage(repository: &RepositoryInput, todo: bool) -> TokenStream2 {
            let sync_ptr = repository.sync_ptr();
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();

            let body = if todo {
                quote! { { todo!() } }
            } else {
                quote! { ; }
            };

            let todo_fns = quote! {
                hex_arch_paste! {
                    fn [<#plural _mut>](&mut self) -> &mut std::collections::HashMap<<Self as [<#ty BaseRepository>]>::Key, (#sync_ptr<#ty>, #sync_ptr<<Self as [<#ty BaseRepository>]>::Record>)>
                    where
                        Self: #read_repositories
                    #body
                }
            };

            if todo {
                quote! {
                    hex_arch_paste! {
                        #todo_fns
                    }
                }
            } else {
                quote! {
                    hex_arch_paste! {
                        #todo_fns

                        fn [<store_ #singular>](&mut self, #singular: <Self as [<#ty BaseRepository>]>::Record)-> (#sync_ptr<#ty>, #sync_ptr<<Self as [<#ty BaseRepository>]>::Record>)
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
                            [<adaptor_ #plural>]
                                .into_iter()
                                .map(|[<adaptor_ #singular>]| {
                                    let key = [<adaptor_ #singular>].as_ref();
                                    let record_and_adaptor_record = if ![<stored_ #plural>].contains_key(key) {
                                        let adaptor_record = [<adaptor_ #singular>].clone();
                                        [<stored_ #plural>]
                                            .entry(key.clone())
                                            .or_insert((
                                                #sync_ptr::new([<adaptor_ #singular>].into()),
                                                #sync_ptr::new(adaptor_record),
                                            ))
                                    } else {
                                        [<stored_ #plural>].get(key).unwrap()
                                    };
                                    (record_and_adaptor_record.0.clone(), record_and_adaptor_record.1.clone())
                                })
                                .unzip()
                        }
                    }
                }
            }
        }

        pub fn get_multiple(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let read_repositories = repository.read_repositories();

            let load_mod = &input.load_relations_trait_mod;

            let load_in_multiples: Vec<_> = repository
                .relations
                .iter()
                .map(|relation| shared::load_in_multiple(repository, relation))
                .collect();

            quote! {
                hex_arch_paste! {
                    fn [<get_ #plural>]<LR: #load_mod::[<Load #ty RelationsTraitRef>]>(
                        &mut self,
                        pre_sort_values: PreSortValues<<Self as [<#ty BaseRepository>]>::Record>,
                        load_relations: &LR,
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<[<#ty Entity>]>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        use #load_mod::*;

                        let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;
                        let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                        let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                                #load_in_multiples
                            }
                        )*

                        Ok(pre_sort_values.into_iter().zip_with(all_loaded_relations, Entity::from).dupe_and_sort::<Self::Error>()?)
                    }

                    fn [<try_get_ #plural>]<LR: #load_mod::[<Load #ty RelationsTraitRef>]>(
                        &mut self,
                        pre_sort_values: PreSortValues<<Self as [<#ty BaseRepository>]>::Record>,
                        load_relations: &LR,
                        client: Self::Client<'_>,
                    ) -> Result<
                        Vec<Option<[<#ty Entity>]>>,
                        Self::Error,
                    >
                    where
                        Self: #read_repositories
                    {
                        use #load_mod::*;

                        let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.try_map_and_take(|x| self.[<store_ #plural>](x));
                        let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                        let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                        #(
                            if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                                #load_in_multiples
                            }
                        )*

                        Ok(pre_sort_values.into_iter().zip_with(all_loaded_relations, Entity::from).try_dupe_and_sort())
                    }
                }
            }
        }

        pub fn load_by_multiple(
            relation: &RelationInput,
            repository: &RepositoryInput,
            relation_repository: &RepositoryInput,
            body: &TokenStream2,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let relation_ty = relation_repository.ty();
            let relation_plural = relation_repository.plural();
            let load_by_multiple_fn_name =
                op_by_multiple_fn_name("load", relation, relation_repository);

            quote! {
                hex_arch_paste! {
                    fn #load_by_multiple_fn_name(
                        #relation_plural: &Vec<&<Self as [<#relation_ty BaseRepository>]>::Record>,
                        client: Self::Client<'_>,
                    ) -> Result<
                        hex_arch::PreSortValues<(
                            <Self as [<#ty BaseRepository>]>::Record,
                            <Self as [<#relation_ty BaseRepository>]>::Key,
                        )>,
                        Self::Error,
                    >
                    where
                        Self: [<#relation_ty ReadRepository>]
                    #body
                }
            }
        }

        pub fn load_by_multiple_keys(
            relation: &RelationInput,
            repository: &RepositoryInput,
            relation_repository: &RepositoryInput,
            body: &TokenStream2,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let relation_key_plural = relation_repository.key_plural();
            let load_by_multiple_keys_fn_name =
                op_by_multiple_keys_fn_name("load", relation, relation_repository);

            let return_ty = quote! { hex_arch_paste! {
                hex_arch::PreSortValues<(
                    <Self as [<#ty BaseRepository>]>::Record,
                    <Self as [<#relation_ty BaseRepository>]>::Key,
                )>
            } };

            quote! {
                hex_arch_paste! {
                    fn #load_by_multiple_keys_fn_name(
                        [<#relation_singular _ #relation_key_plural>]: Vec<<Self as [<#relation_ty BaseRepository>]>::Key>,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    where
                        Self: [<#relation_ty ReadRepository>]
                    #body
                }
            }
        }

        pub fn get_by_single(
            relation: &RelationInput,
            repository: &RepositoryInput,
            relation_repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let relation_load_in_singles: Vec<_> = repository
                .relations
                .iter()
                .map(|relation| shared::load_in_single(repository, relation))
                .collect();
            let relation_load_in_multiples: Vec<_> = repository
                .relations
                .iter()
                .map(|relation| load_in_multiple(repository, relation))
                .collect();

            let relation_ty = relation_repository.ty();
            let relation_singular = relation_repository.singular();
            let read_repositories = repository.read_repositories();

            let get_by_single_fn_name = op_by_single_fn_name("get", relation, relation_repository);
            let load_by_multiple_fn_name =
                op_by_multiple_fn_name("load", relation, relation_repository);

            let load_mod = &input.load_relations_trait_mod;

            let return_ty = match relation.cardinality {
                Cardinality::One => {
                    quote! { hex_arch_paste! {
                        [<#ty Entity>]
                    } }
                }
                Cardinality::OneOrNone => {
                    quote! { hex_arch_paste! {
                        Option<[<#ty Entity>]>
                    } }
                }
                Cardinality::Many | Cardinality::AtLeastOne => {
                    quote! { hex_arch_paste! {
                        Vec<(
                            [<#ty Entity>],
                            <Self as [<#ty BaseRepository>]>::Key,
                            <Self as [<#relation_ty BaseRepository>]>::Key,
                        )>
                    } }
                }
            };

            let body = match relation.cardinality {
                Cardinality::One => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let adaptor_record = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?
                        .take_right()
                        .0
                        .values()
                        .pop()
                        .unwrap();

                    let (record, #singular) = self.[<store_ #singular>](adaptor_record);
                    let #singular = #singular.deref();

                    let mut loaded_relations = [<Loaded #ty Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_singles
                        }
                    )*

                    Ok(Entity {
                        value: record,
                        relations: loaded_relations,
                    })
                } },
                Cardinality::OneOrNone => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let adaptor_record_option = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?
                        .take_right()
                        .0
                        .values()
                        .pop();

                    let adaptor_record = match adaptor_record_option {
                        Some(adaptor_record) => adaptor_record,
                        None => return Ok(None),
                    };

                    let (record, #singular) = self.[<store_ #singular>](adaptor_record);
                    let #singular = #singular.deref();

                    let mut loaded_relations = [<Loaded #ty Relations>]::default();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_singles
                        }
                    )*

                    Ok(Some(Entity {
                        value: record,
                        relations: loaded_relations,
                    }))
                } },
                Cardinality::Many => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let pre_sort_values = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?;

                    let (pre_sort_values, parent_keys) = pre_sort_values.take_right();

                    let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        pre_sort_values
                            .into_iter()
                            .zip_with(all_loaded_relations, Entity::from)
                            .zip(parent_keys)
                            .map(|(entity, parent_key)| {
                                let record_key: <Self as [<#ty BaseRepository>]>::Key = entity.value.deref().as_ref().clone().into();
                                (
                                    entity,
                                    record_key,
                                    parent_key,
                                )
                            })
                            .dupe_and_sort::<Self::Error>()?
                    )
                } },
                Cardinality::AtLeastOne => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let pre_sort_values = Self::#load_by_multiple_fn_name(&vec![#relation_singular], client)?;

                    if pre_sort_values.len() == 0 {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (pre_sort_values, parent_keys) = pre_sort_values.take_right();

                    let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        pre_sort_values
                            .into_iter()
                            .zip_with(all_loaded_relations, Entity::from)
                            .zip(parent_keys)
                            .map(|(entity, parent_key)| {
                                let record_key: <Self as [<#ty BaseRepository>]>::Key = entity.value.deref().as_ref().clone().into();
                                (
                                    entity,
                                    record_key,
                                    parent_key,
                                )
                            })
                            .dupe_and_sort::<Self::Error>()?
                    )
                } },
            };

            quote! {
                hex_arch_paste! {
                    fn #get_by_single_fn_name<LR: #load_mod::[<Load #ty RelationsTraitRef>]>(
                        &mut self,
                        #relation_singular: &<Self as [<#relation_ty BaseRepository>]>::Record,
                        load_relations: &LR,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    where
                        Self: #read_repositories
                    {
                        #body
                    }
                }
            }
        }

        pub fn get_by_multiple(
            relation: &RelationInput,
            repository: &RepositoryInput,
            relation_repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_snakes = repository.relation_snakes();
            let relation_load_in_multiples: Vec<_> = repository
                .relations
                .iter()
                .map(|relation| load_in_multiple(repository, relation))
                .collect();

            let relation_ty = relation_repository.ty();
            let relation_plural = relation_repository.plural();
            let read_repositories = repository.read_repositories();

            let get_by_multiple_fn_name =
                op_by_multiple_fn_name("get", relation, relation_repository);
            let load_by_multiple_fn_name =
                op_by_multiple_fn_name("load", relation, relation_repository);

            let load_mod = &input.load_relations_trait_mod;

            let return_ty = match relation.cardinality {
                Cardinality::One => {
                    quote! { hex_arch_paste! {
                        Vec<[<#ty Entity>]>
                    } }
                }
                Cardinality::OneOrNone => {
                    quote! { hex_arch_paste! {
                        Vec<Option<[<#ty Entity>]>>
                    } }
                }
                Cardinality::Many | Cardinality::AtLeastOne => {
                    quote! { hex_arch_paste! {
                        Vec<(
                            [<#ty Entity>],
                            <Self as [<#ty BaseRepository>]>::Key,
                            <Self as [<#relation_ty BaseRepository>]>::Key,
                        )>
                    } }
                }
            };

            let body = match relation.cardinality {
                Cardinality::One => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let (pre_sort_values, [<#plural _ptrs>]) =  Self::#load_by_multiple_fn_name(&#relation_plural, client)?
                        .into_iter()
                        .map(|(record, _)| record)
                        .map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;

                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(pre_sort_values.into_iter().zip_with(all_loaded_relations, Entity::from).dupe_and_sort::<Self::Error>()?)
                } },
                Cardinality::OneOrNone => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let (pre_sort_values, [<#plural _ptrs>]) =  Self::#load_by_multiple_fn_name(&#relation_plural, client)?
                        .into_iter()
                        .map(|(record, _)| record)
                        .try_map_and_take(|x| self.[<store_ #plural>](x));

                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(pre_sort_values.into_iter().zip_with(all_loaded_relations, Entity::from).try_dupe_and_sort())
                } },
                Cardinality::Many => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let pre_sort_values = Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    let (pre_sort_values, parent_keys) = pre_sort_values.take_right();

                    let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        pre_sort_values
                            .into_iter()
                            .zip_with(all_loaded_relations, Entity::from)
                            .zip(parent_keys)
                            .map(|(entity, parent_key)| {
                                let record_key: <Self as [<#ty BaseRepository>]>::Key = entity.value.deref().as_ref().clone().into();
                                (
                                    entity,
                                    record_key,
                                    parent_key,
                                )
                            })
                            .dupe_and_sort::<Self::Error>()?
                    )
                } },
                Cardinality::AtLeastOne => quote! { hex_arch_paste! {
                    use #load_mod::*;

                    let pre_sort_values = Self::#load_by_multiple_fn_name(&#relation_plural, client)?;

                    let (pre_sort_values, parent_keys) = pre_sort_values.take_right();

                    if parent_keys.iter().unique().collect::<Vec<_>>().len() != #relation_plural.len() {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }

                    let (pre_sort_values, [<#plural _ptrs>]) = pre_sort_values.map_and_take::<_, _, Self::Error>(|x| self.[<store_ #plural>](x))?;
                    let #plural: Vec<_> = [<#plural _ptrs>].iter().map(|[<#plural _ptr>]| [<#plural _ptr>].deref()).collect();

                    let mut all_loaded_relations: Vec<_> = (0..#plural.len()).map(|_| [<Loaded #ty Relations>]::default()).collect();

                    #(
                        if let Some(load_relations) = load_relations.[<#relation_snakes _ref>]() {
                            #relation_load_in_multiples
                        }
                    )*

                    Ok(
                        pre_sort_values
                            .into_iter()
                            .zip_with(all_loaded_relations, Entity::from)
                            .zip(parent_keys)
                            .map(|(entity, parent_key)| {
                                let record_key: <Self as [<#ty BaseRepository>]>::Key = entity.value.deref().as_ref().clone().into();
                                (
                                    entity,
                                    record_key,
                                    parent_key,
                                )
                            })
                            .dupe_and_sort::<Self::Error>()?
                    )
                } },
            };

            quote! {
                hex_arch_paste! {
                    fn #get_by_multiple_fn_name<LR: #load_mod::[<Load #ty RelationsTraitRef>]>(
                        &mut self,
                        #relation_plural: &Vec<&<Self as [<#relation_ty BaseRepository>]>::Record>,
                        load_relations: &LR,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    where
                        Self: #read_repositories
                    {
                        #body
                    }
                }
            }
        }

        pub fn get_load_by_field_multiple_fn_name(
            repository: &RepositoryInput,
            load_by: &LoadByInput,
        ) -> Ident {
            format_ident!("load_{}_by_{}", repository.plural(), load_by.plural())
        }

        pub fn load_by_field_multiple(
            repository: &RepositoryInput,
            load_by: &LoadByInput,
            body: &TokenStream2,
            try_body: &TokenStream2,
        ) -> TokenStream2 {
            let ty = repository.ty();

            let load_by_ty = load_by.ty();
            let load_by_plural = load_by.plural();

            let load_by_field_multiple_fn_name =
                get_load_by_field_multiple_fn_name(repository, load_by);
            let try_load_by_field_multiple_fn_name =
                format_ident!("try_{}", load_by_field_multiple_fn_name);

            let return_ty = match load_by.cardinality {
                Cardinality::One | Cardinality::OneOrNone => {
                    quote! { hex_arch_paste! {
                        hex_arch::PreSortValues<<Self as [<#ty BaseRepository>]>::Record>
                    } }
                }
                Cardinality::Many | Cardinality::AtLeastOne => {
                    quote! { hex_arch_paste! {
                        Vec<<Self as [<#ty BaseRepository>]>::Record>
                    } }
                }
            };

            quote! {
                hex_arch_paste! {
                    fn #load_by_field_multiple_fn_name(
                        #load_by_plural: Vec<#load_by_ty>,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    #body

                    fn #try_load_by_field_multiple_fn_name(
                        #load_by_plural: Vec<#load_by_ty>,
                        client: Self::Client<'_>,
                    ) -> Result<#return_ty, Self::Error>
                    #try_body
                }
            }
        }

        pub fn get_by_field(repository: &RepositoryInput, load_by: &LoadByInput) -> TokenStream2 {
            let ty = repository.ty();
            let read_repositories = repository.read_repositories();

            let load_by_ty = load_by.ty();
            let load_by_singular = load_by.singular();
            let load_by_plural = load_by.plural();

            let load_by_field_multiple_fn_name =
                get_load_by_field_multiple_fn_name(repository, load_by);
            let try_load_by_field_multiple_fn_name =
                format_ident!("try_{}", load_by_field_multiple_fn_name);

            match load_by.cardinality {
                Cardinality::One => quote! { hex_arch_paste! {
                    impl #ty {
                        pub fn [<get_by_ #load_by_singular>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_singular: #load_by_ty,
                        ) -> [<Get #ty Builder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<Get #ty Builder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(Adaptor::#load_by_field_multiple_fn_name(vec![#load_by_singular], client)?)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }

                        pub fn [<get_batch_by_ #load_by_plural>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_plural: Vec<#load_by_ty>,
                        ) -> [<Get #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(#load_by_plural, client)?
                                )),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }

                        pub fn [<try_get_by_ #load_by_singular>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_singular: #load_by_ty,
                        ) -> [<TryGet #ty Builder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<TryGet #ty Builder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(Adaptor::#try_load_by_field_multiple_fn_name(vec![#load_by_singular], client)?)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }

                        pub fn [<try_get_batch_by_ #load_by_plural>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_plural: Vec<#load_by_ty>,
                        ) -> [<TryGet #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<TryGet #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(Adaptor::#try_load_by_field_multiple_fn_name(#load_by_plural.clone(), client)?)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                } },
                Cardinality::Many => quote! { hex_arch_paste! {
                    impl #ty {
                        pub fn [<get_by_ #load_by_singular>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_singular: #load_by_ty
                        ) -> [<Get #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(vec![#load_by_singular], client)?
                                )),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }

                        pub fn [<get_batch_by_ #load_by_plural>]<Adaptor: [<#ty ReadRepository>] + #read_repositories>(
                            #load_by_plural: Vec<#load_by_ty>
                        ) -> [<Get #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            [<Get #ty sBuilder>] {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(
                                    Adaptor::#load_by_field_multiple_fn_name(#load_by_plural, client)?
                                )),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                } },
                _ => unreachable!(),
            }
        }

        pub fn load_keys_by_multiple(
            repository: &RepositoryInput,
            relation: &RelationInput,
            relation_repository: &RepositoryInput,
            body: &TokenStream2,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let relation_ty = relation_repository.ty();
            let relation_key_plural = relation_repository.key_plural();
            let relation_singular = relation_repository.singular();
            let fn_name =
                op_keys_by_multiple_fn_name("load", repository, relation, relation_repository);
            quote! {
                hex_arch_paste! {
                    pub (crate) fn #fn_name(
                        [<#relation_singular _ #relation_key_plural>]: Vec<<Self as [<#relation_ty BaseRepository>]>::Key>,
                        client: <Self as BaseRepository>::Client<'_>,
                    ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Key>, <Self as BaseRepository>::Error> {
                        #body
                    }
                }
            }
        }
    }

    pub mod builders {
        use super::*;

        pub fn get_ty_singular_builder(
            repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let inward_relations = repository.inward_relations(input);

            let builder = format_ident!("Get{}Builder", ty);
            let impl_builder_load_fns = shared::get_impl_read_builder_load_fns(&builder, repository, input);

            let get_bys: Vec<_> = inward_relations
                .iter()
                .filter_map(|(relation, relation_repository)| {
                    if *relation_repository.ty() == *ty {
                        return None
                    }
                    match relation.cardinality {
                        Cardinality::One => {
                            let relation_plural = relation.plural();
                            let relation_ty = relation_repository.ty();
                            let relation_singular = relation_repository.singular();
                            let relation_key_singular = relation_repository.key_singular();
                            let relation_key_plural = relation_repository.key_plural();
                            let fn_name = format_ident!("get_by_{}_{}", relation_singular, relation_key_singular);

                            Some((
                                quote! {
                                    hex_arch_paste! {
                                        impl #ty {
                                            pub fn #fn_name<Adaptor: #read_repositories>(
                                                key: impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>,
                                            ) -> #builder<
                                                Adaptor,
                                                [<StaticLoad #ty Relations>],
                                            > {
                                                #builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(move |client| Ok(
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](vec![key.into()], client)?
                                                            .take_right()
                                                            .0
                                                    )),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    }
                                },
                                fn_name,
                            ))
                        },
                        _ => None,
                    }
                })
                .unique_by(|x| x.1.clone())
                .map(|(x, _)| x)
                .collect();

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: #read_repositories, LR> {
                        adaptor: Adaptor,
                        load_pre_sort_values: Box<
                            dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<
                                PreSortValues<<Adaptor as [<#ty BaseRepository>]>::Record>,
                                <Adaptor as BaseRepository>::Error,
                            >
                        >,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<[<#ty Entity>], <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>]((self.load_pre_sort_values)(client)?, &self.load_relations, client)?
                                .pop()
                                .ok_or_else(<Adaptor as BaseRepository>::Error::not_found)
                        }
                    }

                    impl #ty {
                        pub fn get<Adaptor: #read_repositories>(
                            key: impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>
                        ) -> #builder<
                            Adaptor,
                            [<StaticLoad #ty Relations>],
                        > {
                            #builder {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(Adaptor::[<load_ #plural>](vec![key.into()], client)?)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }

                    #( #get_bys )*
                }
            }
        }

        pub fn get_ty_multiple_builder(
            repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let inward_relations = repository.inward_relations(input);

            let builder = format_ident!("Get{}sBuilder", ty);
            let impl_builder_load_fns = shared::get_impl_read_builder_load_fns(&builder, repository, input);

            let get_batch_bys: Vec<_> = inward_relations
                .iter()
                .filter_map(|(relation, relation_repository)| {
                    if *relation_repository.ty() == *ty {
                        return None
                    }

                    let relation_plural = relation.plural();
                    let relation_ty = relation_repository.ty();
                    let relation_singular = relation_repository.singular();
                    let relation_key_singular = relation_repository.key_singular();
                    let relation_key_plural = relation_repository.key_plural();

                    match relation.cardinality {
                        Cardinality::One => {
                            let fn_name = format_ident!("get_batch_by_{}_{}", relation_singular, relation_key_plural);
                            Some(vec![(
                                quote! {
                                    hex_arch_paste! {
                                        impl #ty {
                                            pub fn #fn_name<Adaptor: #read_repositories>(keys: Vec<impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>>) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                #builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(move |client| Ok(
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](
                                                            keys.into_iter().map(|key| key.into()).collect(),
                                                            client,
                                                        )?
                                                            .take_right()
                                                            .0
                                                    )),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    }
                                },
                                fn_name,
                            )].into_iter())
                        },
                        Cardinality::Many|Cardinality::AtLeastOne => {
                            let relation_key_ty = relation_repository.key_ty();
                            let singular_fn_name = format_ident!("get_batch_by_{}_{}", relation_singular, relation_key_singular);
                            let plural_fn_name = format_ident!("get_batch_by_{}_{}", relation_singular, relation_key_plural);
                            let by_many_builder = format_ident!("Get{}sByMany{}sBuilder", ty, relation_ty);
                            let impl_by_many_builder_load_fns = shared::get_impl_read_builder_load_fns(&by_many_builder, repository, input);

                            Some(vec![
                                (
                                    quote! {
                                        hex_arch_paste! {
                                            impl #ty {
                                                pub fn #singular_fn_name<Adaptor: #read_repositories>(key: impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                    #builder {
                                                        adaptor: Adaptor::default(),
                                                        load_pre_sort_values: Box::new(move |client| Ok(
                                                            Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](vec![key.into()], client)?
                                                                .take_right()
                                                                .0
                                                        )),
                                                        load_relations: [<StaticLoad #ty Relations>]::default(),
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    singular_fn_name,
                                ),
                                (
                                    quote! {
                                        pub struct #by_many_builder<Adaptor: #read_repositories, LR> {
                                            adaptor: Adaptor,
                                            load_pre_sort_values: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<PreSortValues<(<Adaptor as [<#ty BaseRepository>]>::Record, <Adaptor as [<#relation_ty BaseRepository>]>::Key)>, <Adaptor as BaseRepository>::Error>>,
                                            pub load_relations: LR,
                                        }

                                        #impl_by_many_builder_load_fns

                                        impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #by_many_builder<Adaptor, LR>
                                        where
                                            #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                                        {
                                            pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<([<#ty Entity>], #relation_key_ty)>, <Adaptor as BaseRepository>::Error> {
                                                let _read_lock = Adaptor::read()?;
                                                let (pre_sort_values, parent_keys) = (self.load_pre_sort_values)(client)?.take_right();
                                                let entities = self.adaptor.[<get_ #plural>](pre_sort_values, &self.load_relations, client)?;
                                                Ok(
                                                    hex_arch_izip!(
                                                        entities.into_iter(),
                                                        parent_keys.into_iter().map(|parent_key| parent_key.into()),
                                                    )
                                                        .collect()
                                                )
                                            }
                                        }

                                        impl #ty {
                                            pub fn #plural_fn_name<Adaptor: #read_repositories>(keys: Vec<impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>>) -> #by_many_builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                #by_many_builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(move |client|
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](
                                                            keys.into_iter().map(|key| key.into()).collect(),
                                                            client,
                                                        )
                                                    ),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    },
                                    plural_fn_name,
                                ),
                            ].into_iter())
                        },
                        _ => None,
                    }
                })
                .flatten()
                .unique_by(|x| x.1.clone())
                .map(|(x, _)| x)
                .collect();

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: #read_repositories, LR> {
                        adaptor: Adaptor,
                        load_pre_sort_values: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<PreSortValues<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<[<#ty Entity>]>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>]((self.load_pre_sort_values)(client)?, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn get_all<Adaptor: #read_repositories>() -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Adaptor::[<load_all_ #plural>](client)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }

                        pub fn get_batch<Adaptor: #read_repositories>(keys: Vec<impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>>) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Adaptor::[<load_ #plural>](
                                    keys.into_iter().map(|key| key.into()).collect(),
                                    client,
                                )),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }

                    #( #get_batch_bys )*
                }
            }
        }

        pub fn try_get_ty_singular_builder(
            repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let inward_relations = repository.inward_relations(input);

            let builder = format_ident!("TryGet{}Builder", ty);
            let impl_builder_load_fns = shared::get_impl_read_builder_load_fns(&builder, repository, input);

            let try_get_bys: Vec<_> = inward_relations
                .iter()
                .filter_map(|(relation, relation_repository)| {
                    if *relation_repository.ty() == *ty {
                        return None
                    }

                    let relation_plural = relation.plural();
                    let relation_ty = relation_repository.ty();
                    let relation_singular = relation_repository.singular();
                    let relation_key_singular = relation_repository.key_singular();
                    let relation_key_plural = relation_repository.key_plural();

                    match relation.cardinality {
                        Cardinality::OneOrNone => {
                            let fn_name = format_ident!("try_get_by_{}_{}", relation_singular, relation_key_singular);
                            Some((
                                quote! {
                                    hex_arch_paste! {
                                        impl #ty {
                                            pub fn #fn_name<Adaptor: #read_repositories>(
                                                key: impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>,
                                            ) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                #builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(move |client| Ok(
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](vec![key.into()], client)?
                                                            .take_right()
                                                            .0
                                                    )),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    }
                                },
                                fn_name,
                            ))
                        },
                        Cardinality::One => {
                            let fn_name = format_ident!("try_get_by_{}_{}", relation_singular, relation_key_singular);
                            Some((
                                quote! {
                                    hex_arch_paste! {
                                        impl #ty {
                                            pub fn #fn_name<Adaptor: #read_repositories>(
                                                key: impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>,
                                            ) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                #builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(move |client| Ok(
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](vec![key.into()], client)?
                                                            .take_right()
                                                            .0
                                                    )),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    }
                                },
                                fn_name,
                            ))
                        },
                        _ => None,
                    }
                })
                .unique_by(|x| x.1.clone())
                .map(|(x, _)| x)
                .collect();

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: #read_repositories, LR> {
                        adaptor: Adaptor,
                        load_pre_sort_values: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<PreSortValues<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Option<[<#ty Entity>]>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            Ok(
                                self.adaptor.[<try_get_ #plural>]((self.load_pre_sort_values)(client)?, &self.load_relations, client)?
                                    .pop()
                                    .flatten()
                            )
                        }
                    }

                    impl #ty {
                        pub fn try_get<Adaptor: #read_repositories>(key: impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(move |client| Ok(Adaptor::[<load_ #plural>](vec![key.into()], client)?)),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }

                    #( #try_get_bys )*
                }
            }
        }

        pub fn try_get_ty_multiple_builder(
            repository: &RepositoryInput,
            input: &RepositoriesInput,
        ) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let relation_tys = repository.relation_tys();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let inward_relations = repository.inward_relations(input);

            let builder = format_ident!("TryGet{}sBuilder", ty);
            let impl_builder_load_fns = shared::get_impl_read_builder_load_fns(&builder, repository, input);

            let try_get_batch_bys: Vec<_> = inward_relations
                .iter()
                .filter_map(|(relation, relation_repository)| {
                    if *relation_repository.ty() == *ty {
                        return None
                    }

                    let relation_plural = relation.plural();
                    let relation_ty = relation_repository.ty();
                    let relation_singular = relation_repository.singular();
                    let relation_key_plural = relation_repository.key_plural();

                    match relation.cardinality {
                        Cardinality::OneOrNone => {
                            let fn_name = format_ident!("try_get_batch_by_{}_{}", relation_singular, relation_key_plural);
                            Some(vec![(
                                quote! {
                                    hex_arch_paste! {
                                        impl #ty {
                                            pub fn #fn_name<Adaptor: #read_repositories>(
                                                keys: Vec<impl 'static + Into<<Adaptor as [<#relation_ty BaseRepository>]>::Key>>
                                            ) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                                                let keys: Vec<<Adaptor as [<#relation_ty BaseRepository>]>::Key> = keys.into_iter().map(|key| key.into()).collect();
                                                #builder {
                                                    adaptor: Adaptor::default(),
                                                    load_pre_sort_values: Box::new(|client| Ok(
                                                        Adaptor::[<load_ #relation_plural _by_ #relation_singular _ #relation_key_plural>](keys, client)?
                                                            .take_right()
                                                            .0
                                                    )),
                                                    load_relations: [<StaticLoad #ty Relations>]::default(),
                                                }
                                            }
                                        }
                                    }
                                },
                                fn_name,
                            )].into_iter())
                        },
                        _ => None,
                    }
                })
                .flatten()
                .unique_by(|x| x.1.clone())
                .map(|(x, _)| x)
                .collect();

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: #read_repositories, LR> {
                        adaptor: Adaptor,
                        load_pre_sort_values: Box<dyn FnOnce(<Adaptor as BaseRepository>::Client<'_>) -> Result<PreSortValues<<Adaptor as [<#ty BaseRepository>]>::Record>, <Adaptor as BaseRepository>::Error>>,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR>
                    where
                        #(<Adaptor as [<#relation_tys BaseRepository>]>::Record: Into<#relation_tys>,)*
                    {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<Option<[<#ty Entity>]>>, <Adaptor as BaseRepository>::Error> {
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<try_get_ #plural>]((self.load_pre_sort_values)(client)?, &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn try_get_batch<Adaptor: #read_repositories>(keys: Vec<impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>>) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                load_pre_sort_values: Box::new(|client| Adaptor::[<load_ #plural>](
                                    keys.into_iter().map(|key| key.into()).collect(),
                                    client,
                                )),
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }

                    #( #try_get_batch_bys )*
                }
            }
        }
    }
}

pub mod write_repositories {
    use super::*;

    pub fn write_repositories(input: &RepositoriesInput, todo: bool) -> TokenStream2 {
        let write_repositories: Vec<_> = input
            .repositories
            .iter()
            .filter(|repository| {
                if let Mutability::RW = repository.mutability {
                    true
                } else {
                    false
                }
            })
            .map(|repository| {
                let ty_write_repository =
                    ty_write_repository::ty_write_repository(repository, todo);

                if todo {
                    quote! {
                        #ty_write_repository
                    }
                } else {
                    let delete_ty_singular_builder =
                        builders::delete_ty_singular_builder(repository);
                    let delete_ty_multiple_builder =
                        builders::delete_ty_multiple_builder(repository);
                    let insert_ty_singular_builder =
                        builders::insert_ty_singular_builder(repository, input);
                    let insert_ty_multiple_builder =
                        builders::insert_ty_multiple_builder(repository, input);
                    let update_ty_singular_builder =
                        builders::update_ty_singular_builder(repository, input);
                    let update_ty_multiple_builder =
                        builders::update_ty_multiple_builder(repository, input);
                    quote! {
                        #ty_write_repository
                        #delete_ty_singular_builder
                        #delete_ty_multiple_builder
                        #insert_ty_singular_builder
                        #insert_ty_multiple_builder
                        #update_ty_singular_builder
                        #update_ty_multiple_builder
                    }
                }
            })
            .collect();

        quote! {
            #(#write_repositories)*
        }
    }

    pub mod ty_write_repository {
        use super::*;

        pub fn ty_write_repository(repository: &RepositoryInput, todo: bool) -> TokenStream2 {
            let ty = repository.ty();
            let singular = repository.singular();
            let plural = repository.plural();
            let key_plural = repository.key_plural();

            let body = if todo {
                quote! { { todo!() } }
            } else {
                quote! { ; }
            };

            quote! {
                hex_arch_paste! {
                    pub trait [<#ty WriteRepository>]: [<#ty ReadRepository>] {
                        fn [<delete_ #plural>](
                            [<#singular _ #key_plural>]: Vec<<Self as [<#ty BaseRepository>]>::Key>,
                            client: Self::Client<'_>,
                        ) -> Result<usize, Self::Error>
                            #body

                        fn [<insert_ #plural>](
                            [<#singular _posts>]: Vec<[<#ty Post>]>,
                            client: Self::Client<'_>,
                        ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error>
                            #body

                        fn [<update_ #plural>](
                            [<#singular _patches>]: Vec<[<#ty Patch>]>,
                            client: Self::Client<'_>,
                        ) -> Result<Vec<<Self as [<#ty BaseRepository>]>::Record>, Self::Error>
                            #body
                    }
                }
            }
        }
    }

    pub mod builders {
        use super::*;

        pub fn delete_ty_singular_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();

            quote! {
                hex_arch_paste! {
                    pub struct [<Delete #ty Builder>]<Adaptor: [<#ty WriteRepository>]> {
                        key: <Adaptor as [<#ty BaseRepository>]>::Key,
                    }

                    impl<Adaptor: [<#ty WriteRepository>]> [<Delete #ty Builder>]<Adaptor> {
                        pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                            let _write_lock = Adaptor::write()?;
                            Adaptor::[<delete_ #plural>](vec![self.key], client)
                        }
                    }

                    impl #ty {
                        pub fn delete<Adaptor: [<#ty WriteRepository>]>(key: impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>) -> [<Delete #ty Builder>]<Adaptor> {
                            [<Delete #ty Builder>] {
                                key: key.into(),
                            }
                        }
                    }
                }
            }
        }

        pub fn delete_ty_multiple_builder(repository: &RepositoryInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();

            quote! {
                hex_arch_paste! {
                    pub struct [<Delete #ty sBuilder>]<Adaptor: [<#ty WriteRepository>]> {
                        keys: Vec<<Adaptor as [<#ty BaseRepository>]>::Key>,
                    }

                    impl<Adaptor: [<#ty WriteRepository>]> [<Delete #ty sBuilder>]<Adaptor> {
                        pub fn run(self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<usize, <Adaptor as BaseRepository>::Error> {
                            let _write_lock = Adaptor::write()?;
                            Adaptor::[<delete_ #plural>](self.keys, client)
                        }
                    }

                    impl #ty {
                        pub fn delete_batch<Adaptor: [<#ty WriteRepository>]>(keys: Vec<impl 'static + Into<<Adaptor as [<#ty BaseRepository>]>::Key>>) -> [<Delete #ty sBuilder>]<Adaptor> {
                            [<Delete #ty sBuilder>] {
                                keys: keys.into_iter().map(|key| key.into()).collect(),
                            }
                        }
                    }
                }
            }
        }

        pub fn insert_ty_singular_builder(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let builder = format_ident!("Insert{}Builder", ty);
            let impl_builder_load_fns = shared::get_impl_write_builder_load_fns(&builder, repository, input, vec!["adaptor", "post"]);

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR> {
                        adaptor: Adaptor,
                        post: [<#ty Post>],
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<[<#ty Entity>], <Adaptor as BaseRepository>::Error> {
                            let adaptor_record = {
                                let _write_lock = Adaptor::write()?;
                                Adaptor::[<insert_ #plural>](vec![self.post], client)?.pop().unwrap()
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](PreSortValues::from(vec![adaptor_record]), &self.load_relations, client)?
                                .pop()
                                .ok_or_else(<Adaptor as BaseRepository>::Error::not_found)
                        }
                    }

                    impl #ty {
                        pub fn insert<Adaptor: [<#ty WriteRepository>] + #read_repositories>(post: [<#ty Post>]) -> #builder<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                post,
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub fn insert_ty_multiple_builder(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let builder = format_ident!("Insert{}sBuilder", ty);
            let impl_builder_load_fns = shared::get_impl_write_builder_load_fns(&builder, repository, input, vec!["adaptor", "posts"]);

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR> {
                        adaptor: Adaptor,
                        posts: Vec<[<#ty Post>]>,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<[<#ty Entity>]>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_records = {
                                let _write_lock = Adaptor::write()?;
                                Adaptor::[<insert_ #plural>](self.posts, client)?
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](PreSortValues::from(adaptor_records), &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn insert_batch<Adaptor: [<#ty WriteRepository>] + #read_repositories>(posts: Vec<[<#ty Post>]>) -> [<Insert #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                posts,
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub fn update_ty_singular_builder(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let builder = format_ident!("Update{}Builder", ty);
            let impl_builder_load_fns = shared::get_impl_write_builder_load_fns(&builder, repository, input, vec!["adaptor", "patch"]);

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR> {
                        adaptor: Adaptor,
                        patch: [<#ty Patch>],
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<[<#ty Entity>], <Adaptor as BaseRepository>::Error> {
                            let adaptor_record = {
                                let _write_lock = Adaptor::write()?;
                                Adaptor::[<update_ #plural>](vec![self.patch], client)?.pop().unwrap()
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](PreSortValues::from(vec![adaptor_record]), &self.load_relations, client)?
                                .pop()
                                .ok_or_else(<Adaptor as BaseRepository>::Error::not_found)
                        }
                    }

                    impl #ty {
                        pub fn update<Adaptor: [<#ty WriteRepository>] + #read_repositories>(patch: [<#ty Patch>]) -> [<Update #ty Builder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                patch,
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }

        pub fn update_ty_multiple_builder(repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
            let ty = repository.ty();
            let plural = repository.plural();
            let read_repositories = repository.read_repositories();
            let load_mod = &input.load_relations_trait_mod;

            let builder = format_ident!("Update{}sBuilder", ty);
            let impl_builder_load_fns = shared::get_impl_write_builder_load_fns(&builder, repository, input, vec!["adaptor", "patches"]);

            quote! {
                hex_arch_paste! {
                    pub struct #builder<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR> {
                        adaptor: Adaptor,
                        patches: Vec<[<#ty Patch>]>,
                        pub load_relations: LR,
                    }

                    #impl_builder_load_fns

                    impl<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR: #load_mod::[<Load #ty RelationsTraitRef>]> #builder<Adaptor, LR> {
                        pub fn run(mut self, client: <Adaptor as BaseRepository>::Client<'_>) -> Result<Vec<[<#ty Entity>]>, <Adaptor as BaseRepository>::Error> {
                            let adaptor_records = {
                                let _write_lock = Adaptor::write()?;
                                Adaptor::[<update_ #plural>](self.patches, client)?
                            };
                            let _read_lock = Adaptor::read()?;
                            self.adaptor.[<get_ #plural>](PreSortValues::from(adaptor_records), &self.load_relations, client)
                        }
                    }

                    impl #ty {
                        pub fn update_batch<Adaptor: [<#ty WriteRepository>] + #read_repositories>(patches: Vec<[<#ty Patch>]>) -> [<Update #ty sBuilder>]<Adaptor, [<StaticLoad #ty Relations>]> {
                            #builder {
                                adaptor: Adaptor::default(),
                                patches,
                                load_relations: [<StaticLoad #ty Relations>]::default(),
                            }
                        }
                    }
                }
            }
        }
    }
}

pub mod shared {
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
    pub fn op_by_single_fn_name(
        op: &str,
        relation: &RelationInput,
        relation_repository: &RepositoryInput,
    ) -> Ident {
        let relation_plural = relation.plural(); // cities | avenues
        let relation_ty_singular = relation_repository.singular(); // road | city
        format_ident!("{}_{}_by_{}", op, relation_plural, relation_ty_singular)
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
    pub fn op_by_multiple_fn_name(
        op: &str,
        relation: &RelationInput,
        relation_repository: &RepositoryInput,
    ) -> Ident {
        let relation_plural = relation.plural(); // cities | avenues
        let relation_ty_plural = relation_repository.plural(); // roads | cities
        format_ident!("{}_{}_by_{}", op, relation_plural, relation_ty_plural)
    }

    pub fn op_by_multiple_keys_fn_name(
        op: &str,
        relation: &RelationInput,
        relation_repository: &RepositoryInput,
    ) -> Ident {
        let relation_plural = relation.plural();
        let relation_ty_singular = relation_repository.singular();
        let relation_ty_key_plural = relation_repository.key_plural();
        format_ident!(
            "{}_{}_by_{}_{}",
            op,
            relation_plural,
            relation_ty_singular,
            relation_ty_key_plural
        )
    }

    pub fn op_keys_by_multiple_fn_name(
        op: &str,
        repository: &RepositoryInput,
        relation: &RelationInput,
        relation_repository: &RepositoryInput,
    ) -> Ident {
        let relation_snake = relation.snake();
        let key_plural = repository.key_plural();
        let relation_ty_singular = relation_repository.singular();
        let relation_ty_key_plural = relation_repository.key_plural();
        format_ident!(
            "{}_{}_{}_by_{}_{}",
            op,
            relation_snake,
            key_plural,
            relation_ty_singular,
            relation_ty_key_plural
        )
    }

    pub fn load_in_single(repository: &RepositoryInput, relation: &RelationInput) -> TokenStream2 {
        let singular = repository.singular();
        let relation_snake = relation.snake();
        let get_by_single_fn_name = op_by_single_fn_name("get", relation, repository);

        match relation.cardinality {
            Cardinality::One => quote! {
                hex_arch_paste! {
                    let related_entity = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    loaded_relations.#relation_snake = Some(Box::new(related_entity));
                }
            },
            Cardinality::OneOrNone => quote! {
                hex_arch_paste! {
                    let related_entity_option = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    loaded_relations.#relation_snake = Some(Box::new(related_entity_option));
                }
            },
            Cardinality::Many => quote! {
                hex_arch_paste! {
                    let related_entities_and_keys = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    let related_entities: Vec<_> = related_entities_and_keys.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                    loaded_relations.#relation_snake = Some(Box::new(related_entities));
                }
            },
            Cardinality::AtLeastOne => quote! {
                hex_arch_paste! {
                    let related_entities_and_keys = self.#get_by_single_fn_name(&#singular, load_relations, client)?;
                    let related_entities: Vec<_> = related_entities_and_keys.into_iter().map(|(related_entity, _, _)| related_entity).collect();
                    if related_entities.len() == 0 {
                        return Err(<<Self as BaseRepository>::Error as RepositoryError>::not_found());
                    }
                    loaded_relations.#relation_snake = Some(Box::new(related_entities));
                }
            },
        }
    }

    pub fn load_in_multiple(
        repository: &RepositoryInput,
        relation: &RelationInput,
    ) -> TokenStream2 {
        let ty = repository.ty();
        let singular = repository.singular();
        let plural = repository.plural();
        let relation_snake = relation.snake();
        let get_by_multiple_fn_name = op_by_multiple_fn_name("get", relation, repository);
        let relation_ty = relation.ty();
        let sync_ptr = relation.sync_ptr();

        match relation.cardinality {
            Cardinality::One => quote! {
                hex_arch_paste! {
                    let related_entities = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    for (loaded_relations, related_entity) in hex_arch_izip!(all_loaded_relations.iter_mut(), related_entities.into_iter()) {
                        loaded_relations.#relation_snake = Some(Box::new(related_entity));
                    }
                }
            },
            Cardinality::OneOrNone => quote! {
                hex_arch_paste! {
                    let related_entity_options = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    for (loaded_relations, related_entity_option) in hex_arch_izip!(all_loaded_relations.iter_mut(), related_entity_options.into_iter()) {
                        loaded_relations.#relation_snake = Some(Box::new(related_entity_option));
                    }
                }
            },
            Cardinality::Many => quote! {
                hex_arch_paste! {
                    let all_related_entities_and_keys = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    let mut all_related_entities_by_parent_keys: hex_arch_indexmap::IndexMap<
                        <Self as [<#ty BaseRepository>]>::Key,
                        hex_arch_indexmap::IndexMap<
                            <Self as [<#relation_ty BaseRepository>]>::Key,
                            Vec<Entity<#sync_ptr<#relation_ty>, [<Loaded #relation_ty Relations>]>>,
                        >,
                    > = hex_arch_indexmap::IndexMap::default();

                    let mut parent_key_counts: std::collections::HashMap<<Self as [<#ty BaseRepository>]>::Key, usize> = std::collections::HashMap::default();
                    for #singular in #plural.iter() {
                        parent_key_counts.entry(#singular.as_ref().clone())
                            .and_modify(|count| *count += 1)
                            .or_insert(1);
                    }

                    for (related_entity, related_key, parent_key) in all_related_entities_and_keys.into_iter() {
                        if !all_related_entities_by_parent_keys.contains_key(&parent_key) {
                            all_related_entities_by_parent_keys.insert(parent_key.clone(), hex_arch_indexmap::IndexMap::default());
                        }

                        let all_related_entities_by_related_keys = all_related_entities_by_parent_keys.get_mut(&parent_key).unwrap();

                        if !all_related_entities_by_related_keys.contains_key(&related_key) {
                            let parent_key_count = parent_key_counts.get(&parent_key).unwrap();
                            all_related_entities_by_related_keys.insert(related_key, dupe((related_entity, *parent_key_count)));
                        } else {
                            let parent_key_count = parent_key_counts.get(&parent_key).unwrap();
                            let related_entities = all_related_entities_by_related_keys.get_mut(&related_key).unwrap();
                            for dupe in dupe_iter((related_entity, *parent_key_count)) {
                                related_entities.push(dupe);
                            }
                        }
                    }

                    for (i, loaded_relations) in all_loaded_relations.iter_mut().enumerate() {
                        loaded_relations.#relation_snake = Some(Box::new(match all_related_entities_by_parent_keys.get_mut(#plural[i].as_ref()) {
                            Some(all_related_entities_by_related_keys) => {
                                all_related_entities_by_related_keys
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
                hex_arch_paste! {
                    let all_related_entities_and_keys = self.#get_by_multiple_fn_name(&#plural, load_relations, client)?;

                    let mut all_related_entities_by_parent_keys: hex_arch_indexmap::IndexMap<
                        <Self as [<#ty BaseRepository>]>::Key,
                        hex_arch_indexmap::IndexMap<
                            <Self as [<#relation_ty BaseRepository>]>::Key,
                            Vec<Entity<#sync_ptr<#relation_ty>, [<Loaded #relation_ty Relations>]>>,
                        >,
                    > = hex_arch_indexmap::IndexMap::default();

                    let mut parent_key_counts: std::collections::HashMap<<Self as [<#ty BaseRepository>]>::Key, usize> = std::collections::HashMap::default();
                    for #singular in #plural.iter() {
                        parent_key_counts.entry(#singular.as_ref().clone())
                            .and_modify(|count| *count += 1)
                            .or_insert(1);
                    }

                    for (related_entity, related_key, parent_key) in all_related_entities_and_keys.into_iter() {
                        if !all_related_entities_by_parent_keys.contains_key(&parent_key) {
                            all_related_entities_by_parent_keys.insert(parent_key.clone(), hex_arch_indexmap::IndexMap::default());
                        }

                        let all_related_entities_by_related_keys = all_related_entities_by_parent_keys.get_mut(&parent_key).unwrap();

                        if !all_related_entities_by_related_keys.contains_key(&related_key) {
                            let parent_key_count = parent_key_counts.get(&parent_key).unwrap();
                            all_related_entities_by_related_keys.insert(related_key, dupe((related_entity, *parent_key_count)));
                        } else {
                            let parent_key_count = parent_key_counts.get(&parent_key).unwrap();
                            let related_entities = all_related_entities_by_related_keys.get_mut(&related_key).unwrap();
                            for dupe in dupe_iter((related_entity, *parent_key_count)) {
                                related_entities.push(dupe);
                            }
                        }
                    }

                    for (i, loaded_relations) in all_loaded_relations.iter_mut().enumerate() {
                        loaded_relations.#relation_snake = Some(Box::new(match all_related_entities_by_parent_keys.get_mut(#plural[i].as_ref()) {
                            Some(all_related_entities_by_related_keys) => {
                                let related_entities: Vec<_> = all_related_entities_by_related_keys
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

    pub (crate) fn get_impl_read_builder_load_fns(builder: &Ident, repository: &RepositoryInput, input: &RepositoriesInput) -> TokenStream2 {
        let ty = repository.ty();
        let relation_tys = repository.relation_tys();
        let relation_snakes = repository.relation_snakes();
        let relation_pascals = repository.relation_pascals();
        let read_repositories = repository.read_repositories();
        let load_mod = &input.load_relations_trait_mod;

        quote! {
            hex_arch_paste! {
                impl<Adaptor: #read_repositories, LR: #load_mod::[<Load #ty RelationsTrait>]> #builder<Adaptor, LR> {

                    pub fn as_dyn(self) -> #builder<Adaptor, [<DynLoad #ty Relations>]> {
                        use #load_mod::*;

                        #builder {
                            load_relations: self.load_relations.as_dyn().unwrap(),
                            adaptor: self.adaptor,
                            load_pre_sort_values: self.load_pre_sort_values,
                        }
                    }

                    pub fn map_load_relations<NewLR: #load_mod::[<Load #ty RelationsTrait>]>(self, map_fn: impl FnOnce(LR) -> NewLR) -> #builder<Adaptor, NewLR> {
                        #builder {
                            load_relations: map_fn(self.load_relations),
                            adaptor: self.adaptor,
                            load_pre_sort_values: self.load_pre_sort_values,
                        }
                    }

                    pub fn r#where<NewWhereExpr>(self, where_expr: NewWhereExpr) -> #builder<
                        Adaptor,
                        LR::WithWhere<NewWhereExpr>,
                    > {
                        #builder {
                            adaptor: self.adaptor,
                            load_pre_sort_values: self.load_pre_sort_values,
                            load_relations: self.load_relations.r#where(where_expr),
                        }
                    }

                    pub fn paginate(mut self, paginate: Paginate<LR::Cursor>) -> Self {
                        self.load_relations = self.load_relations.paginate(paginate);
                        self
                    }

                    #(
                        pub fn [<load_ #relation_snakes>](self) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]>,
                        > {
                            #builder {
                                adaptor: self.adaptor,
                                load_pre_sort_values: self.load_pre_sort_values,
                                load_relations: self.load_relations.[<load_ #relation_snakes>](),
                            }
                        }

                        pub fn [<load_ #relation_snakes _with>]<SubLR: #load_mod::[<Load #relation_tys RelationsTrait>]>(
                            self,
                            with_fn: impl FnOnce(<LR as #load_mod::[<Load #ty RelationsTrait>]>::[<Load #relation_pascals Relations>]) -> SubLR,
                        ) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<SubLR>,
                        > {
                            #builder {
                                adaptor: self.adaptor,
                                load_pre_sort_values: self.load_pre_sort_values,
                                load_relations: self.load_relations.[<load_ #relation_snakes _with>](with_fn),
                            }
                        }

                        pub fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            self,
                            where_expr: NewWhereExpr,
                        ) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>>,
                        > {
                            #builder {
                                adaptor: self.adaptor,
                                load_pre_sort_values: self.load_pre_sort_values,
                                load_relations: self.load_relations.[<load_ #relation_snakes _where>](where_expr),
                            }
                        }
                    )*
                }
            }
        }
    }

    pub (crate) fn get_impl_write_builder_load_fns(builder: &Ident, repository: &RepositoryInput, input: &RepositoriesInput, other_fields: Vec<&'static str>) -> TokenStream2 {
        let ty = repository.ty();
        let relation_tys = repository.relation_tys();
        let relation_snakes = repository.relation_snakes();
        let relation_pascals = repository.relation_pascals();
        let read_repositories = repository.read_repositories();
        let load_mod = &input.load_relations_trait_mod;

        let other_fields: Vec<_> = other_fields.iter().map(|field| format_ident!("{}", field)).collect();
        let duped_other_fields: Vec<Vec<_>> = other_fields.iter().map(|_| other_fields.clone()).collect();

        quote! {
            hex_arch_paste! {
                impl<Adaptor: [<#ty WriteRepository>] + #read_repositories, LR: #load_mod::[<Load #ty RelationsTrait>]> #builder<Adaptor, LR> {

                    pub fn as_dyn(self) -> #builder<Adaptor, [<DynLoad #ty Relations>]> {
                        use #load_mod::*;

                        #builder {
                            load_relations: self.load_relations.as_dyn().unwrap(),
                            #( #other_fields: self.#other_fields, )*
                        }
                    }

                    pub fn map_load_relations<NewLR: #load_mod::[<Load #ty RelationsTrait>]>(self, map_fn: impl FnOnce(LR) -> NewLR) -> #builder<Adaptor, NewLR> {
                        #builder {
                            load_relations: map_fn(self.load_relations),
                            #( #other_fields: self.#other_fields, )*
                        }
                    }

                    pub fn r#where<NewWhereExpr>(self, where_expr: NewWhereExpr) -> #builder<
                        Adaptor,
                        LR::WithWhere<NewWhereExpr>,
                    > {
                        #builder {
                            load_relations: self.load_relations.r#where(where_expr),
                            #( #other_fields: self.#other_fields, )*
                        }
                    }

                    pub fn paginate(mut self, paginate: Paginate<LR::Cursor>) -> Self {
                        self.load_relations = self.load_relations.paginate(paginate);
                        self
                    }

                    #(
                        pub fn [<load_ #relation_snakes>](self) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]>,
                        > {
                            #builder {
                                #( #duped_other_fields: self.#duped_other_fields, )*
                                load_relations: self.load_relations.[<load_ #relation_snakes>](),
                            }
                        }

                        pub fn [<load_ #relation_snakes _with>]<SubLR: #load_mod::[<Load #relation_tys RelationsTrait>]>(
                            self,
                            with_fn: impl FnOnce(<LR as #load_mod::[<Load #ty RelationsTrait>]>::[<Load #relation_pascals Relations>]) -> SubLR,
                        ) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<SubLR>,
                        > {
                            #builder {
                                #( #duped_other_fields: self.#duped_other_fields, )*
                                load_relations: self.load_relations.[<load_ #relation_snakes _with>](with_fn),
                            }
                        }

                        pub fn [<load_ #relation_snakes _where>]<NewWhereExpr: 'static>(
                            self,
                            where_expr: NewWhereExpr,
                        ) -> #builder<
                            Adaptor,
                            LR::[<WithLoad #relation_pascals Relations>]<[<StaticLoad #relation_tys Relations>]<NewWhereExpr>>,
                        > {
                            #builder {
                                #( #duped_other_fields: self.#duped_other_fields, )*
                                load_relations: self.load_relations.[<load_ #relation_snakes _where>](where_expr),
                            }
                        }
                    )*
                }
            }
        }
    }
}
