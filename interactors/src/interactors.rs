#[macro_export]
macro_rules! get {
    ($ty:ident $plural:ident $key_ty:ty) => {
        hex_arch_paste! {
            pub fn [<try_get_ $plural>](
                context: InteractorContext<Client, KeyValueClient>,
                keys: Vec<$key_ty>,
                load: impl Send + Fn([<StaticLoad $ty Relations>]) -> [<StaticDynLoad $ty Relations>],
            ) -> Result<Vec<Option<[<$ty Entity>]>>, InteractorError<EntityError>> {
                Ok(
                    $ty::try_get_batch::<Adaptor>(keys)
                        .map_load_relations(load)
                        .run(context.client.into())?
                )
            }

            pub fn [<get_all_ $plural>](
                context: InteractorContext<Client, KeyValueClient>,
                load: impl Send + Fn([<StaticLoad $ty Relations>]) -> [<StaticDynLoad $ty Relations>],
            ) -> Result<Vec<[<$ty Entity>]>, InteractorError<EntityError>> {
                Ok(
                    $ty::get_all::<Adaptor>()
                        .map_load_relations(load)
                        .run(context.client.into())?
                )
            }

            pub fn [<try_get_ $plural _by_slugs>](
                context: InteractorContext<Client, KeyValueClient>,
                slugs: Vec<Uuid>,
                load: impl Send + Fn([<StaticLoad $ty Relations>]) -> [<StaticDynLoad $ty Relations>],
            ) -> Result<Vec<Option<[<$ty Entity>]>>, InteractorError<EntityError>> {
                Ok(
                    $ty::try_get_batch_by_slugs::<Adaptor>(slugs)
                        .map_load_relations(load)
                        .run(context.client.into())?
                )
            }
        }
    };
}

#[macro_export]
macro_rules! make_mutate {
    ($api_prefix:ident $api_ty_suffix:ident $internal_prefix:ident $ty:ident $plural:ident $($caches:ident)?) => {
        hex_arch_paste! {
            pub fn [<$api_prefix _ $plural>](
                context: InteractorContext<Client, KeyValueClient>,
                batch: Vec<models::[<$ty $api_ty_suffix>]>,
                load: impl Send + Fn([<StaticLoad $ty Relations>]) -> [<StaticDynLoad $ty Relations>],
            ) -> Result<Vec<[<$ty Entity>]>, InteractorError<[<$ty $api_ty_suffix Error>]>>
            where
                InteractorError<[<$ty $api_ty_suffix Error>]>: From<<Client as Transactional>::AdaptorError>,
            {
                let client = context.client.into();
                let conns: Connections<Adaptor> = Connections(client, Arc::new(Mutex::new(context.key_value_client)));

                let batch = batch
                    .into_iter()
                    .map(|x| x.connect(&conns).try_into().map_err(InteractorError::invalid))
                    .collect::<Result<Vec<_>, _>>()?;

                let $plural = context.client.with_transaction(||
                    $ty::[<$internal_prefix _batch>]::<Adaptor>(batch)
                        .map_load_relations(load)
                        .run(client)
                )?;

                $(
                    let mut $caches = caches();
                    let mut key_value_client = conns.1.lock().unwrap();
                    $caches.[<reload_ $plural>]::<Adaptor, KeyValueClient>((conns.0.into(), &mut key_value_client)).unwrap();
                )?

                Ok($plural)
            }
        }
    };
}

#[macro_export]
macro_rules! mutate {
    ($ty:ident $plural:ident $key_ty:ty) => {
        make_mutate! { create Post insert $ty $plural }
        make_mutate! { update Patch update $ty $plural }

        hex_arch_paste! {
            pub fn [<delete_ $plural>](context: InteractorContext<Client, KeyValueClient>, keys: Vec<$key_ty>) -> Result<usize, InteractorError<EntityError>> {
                let client = context.client.into();
                let num_deleted = context.client.with_transaction(|| $ty::delete_batch::<Adaptor>(keys).run(client))?;
                Ok(num_deleted)
            }
        }
    };
}

#[macro_export]
macro_rules! mutate_with_cache {
    ($ty:ident $plural:ident $key_ty:ty) => {
        make_mutate! { create Post insert $ty $plural caches }
        make_mutate! { update Patch update $ty $plural caches }

        hex_arch_paste! {
            pub fn [<delete_ $plural>](context: InteractorContext<Client, KeyValueClient>, keys: Vec<$key_ty>) -> Result<usize, InteractorError<EntityError>> {
                let client = context.client.into();
                let conns: Connections<Adaptor> = Connections(client, Arc::new(Mutex::new(context.key_value_client)));

                let num_deleted = context.client.with_transaction(|| $ty::delete_batch::<Adaptor>(keys).run(client))?;

                let mut caches = caches();
                let mut key_value_client = conns.1.lock().unwrap();
                caches.[<reload_ $plural>]::<Adaptor, KeyValueClient>((conns.0.into(), &mut key_value_client)).unwrap();

                Ok(num_deleted)
            }
        }
    };
}

#[macro_export]
macro_rules! crud {
    (R $ty:ident $plural:ident $key_ty:ty) => {
        get! { $ty $plural $key_ty }
    };

    (RW $ty:ident $plural:ident $key_ty:ty) => {
        get! { $ty $plural $key_ty }
        mutate! { $ty $plural $key_ty }
    };

    (RWC $ty:ident $plural:ident $key_ty:ty) => {
        get! { $ty $plural $key_ty }
        mutate_with_cache! { $ty $plural $key_ty }
    };
}
