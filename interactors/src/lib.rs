#![allow(incomplete_features)]
#![feature(specialization)]

#[macro_use]
extern crate cfg_if;

#[macro_use]
extern crate serde_with;

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


#[serde_as]
#[derive(Clone, Debug, Serialize)]
pub enum InteractorError<E> {
    Entity(EntityError),
    InvalidInput(E),
    Other,
    Unauthorized,
}

impl<E> std::fmt::Display for InteractorError<E> {
    default fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            InteractorError::Entity(err) => write!(f, "{}", err),
            InteractorError::InvalidInput(_) => write!(f, "Invalid input"),
            InteractorError::Other => write!(f, "Internal server error"),
            InteractorError::Unauthorized => write!(f, "Unauthorized"),
        }
    }
}

impl<E: std::fmt::Display> std::fmt::Display for InteractorError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            InteractorError::Entity(err) => write!(f, "{}", err),
            InteractorError::InvalidInput(err) => write!(f, "{}", err),
            InteractorError::Other => write!(f, "Internal server error"),
            InteractorError::Unauthorized => write!(f, "Unauthorized"),
        }
    }
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
            InteractorError::Entity(_) => 422,
            InteractorError::InvalidInput(_) => 422,
            InteractorError::Other => 500,
            InteractorError::Unauthorized => 401,
        }
    }
}

impl<E> From<DieselError> for InteractorError<E> {
    fn from(_: DieselError) -> Self {
        InteractorError::Other
    }
}

impl<E> From<EntityError> for InteractorError<E> {
    fn from(err: EntityError) -> Self {
        if let EntityError::Poison = err {
            return InteractorError::Other
        }
        InteractorError::Entity(err)
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
