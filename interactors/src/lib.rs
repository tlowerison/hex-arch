#[macro_use]
extern crate cfg_if;

pub use convert_case as interactors_convert_case;
pub use paste::paste as interactors_paste;
pub use proc_macros::*;

use diesel::result::Error as DieselError;
use entities::EntityError;
use serde::Serialize;

cfg_if! {
    if #[cfg(feature = "graphql")] {
        pub use juniper as interactors_juniper;

        pub trait GraphQLLoad {
            type LoadRelations: Sized;
            fn load<'a, S: 'a>(selection: &juniper::LookAheadSelection<'a, S>) -> Box<dyn Send + Fn(Self::LoadRelations) -> Self::LoadRelations>;
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum InteractorError<E> {
    InvalidInput(E),
    Unauthorized,
    Other,
}

impl<E> Default for InteractorError<E> {
    fn default() -> Self {
        InteractorError::Other
    }
}

impl<E> InteractorError<E> {
    pub fn invalid<T: Into<E>>(err: T) -> InteractorError<E> {
        InteractorError::InvalidInput(err.into())
    }

    pub fn status(&self) -> u16 {
        match self {
            InteractorError::InvalidInput(_) => 422,
            InteractorError::Unauthorized => 401,
            InteractorError::Other => 500,
        }
    }
}

impl<E: std::fmt::Debug> std::fmt::Display for InteractorError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:#?}", self)
    }
}

impl<E> From<DieselError> for InteractorError<E> {
    fn from(_: DieselError) -> Self {
        InteractorError::Other
    }
}

impl<E> From<EntityError> for InteractorError<E> {
    fn from(_: EntityError) -> Self {
        InteractorError::Other
    }
}

pub struct Context<Client, KeyValueClient> {
    pub client: Client,
    pub key_value_client: KeyValueClient,
}

impl<Client, KeyValueClient> Context<Client, KeyValueClient> {
    pub fn new(
        client: Client,
        key_value_client: KeyValueClient,
    ) -> Context<Client, KeyValueClient> {
        Context {
            client,
            key_value_client,
        }
    }

    pub fn with<Q: Into<Client>, P: Into<KeyValueClient>>(
        q: Q,
        p: P,
    ) -> Context<Client, KeyValueClient> {
        Context {
            client: q.into(),
            key_value_client: p.into(),
        }
    }
}

#[macro_export]
macro_rules! make_interactor {
    (
        $namespace:ident
        impl<$adaptor:ident, $client:ident> $interactor:ident<$tt1:tt, $tt2:tt>
        $(where $($trait_ty:ty: $trait_bound:ty),+)?
        {
            $(
                pub fn $fn_name:ident$(<$($generic:ident: $trai:ty),+$(,)?>)?($($context:ident)+: Context$(, $($arg_name:ident)+: $arg_ty:ty)*$(,)?) -> Result<$ok_ty:ty, $err_ty:ty>
                $(where { $($tt:tt)* })?
                $block:block
            )*
            $($rest:tt)*
        }
    ) => {
        interactors_paste! {
            impl<'a, $adaptor: 'a, $client> $interactor<$tt1, $tt2>
            $(where $($trait_ty: $trait_bound),+)?
            {
                $(
                    pub fn $fn_name$(<$($generic: $trai),+>)?(
                        $($context)+: Context<$client>
                        $(, $($arg_name)+: $arg_ty)*
                    ) -> Result<$ok_ty, $err_ty>
                    where
                        $adaptor: entities::[<$namespace ReadWriteRepository>]<Error = EntityError>,
                        $client: Copy + Transactional + Into<<$adaptor as hex_arch::BaseRepository>::$client<'a>>,
                        $err_ty: From<<$client as Transactional>::AdaptorError>,
                        <$adaptor as hex_arch::BaseRepository>::Error: From<<$client as Transactional>::AdaptorError>,
                        $($($tt)*)?
                    $block
                )*

                $($rest)*
            }
        }
    };
}
