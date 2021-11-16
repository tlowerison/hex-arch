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
