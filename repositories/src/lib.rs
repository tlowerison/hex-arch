#![feature(generic_associated_types)]

pub use common::transpose_2 as repositories_transpose_2;
pub use itertools::{izip as repositories_izip, Itertools as RepositoriesItertools};
pub use paste::paste as repositories_paste;
pub use proc_macros::*;

#[derive(Clone, Debug)]
pub struct Entity<E, R> {
    pub value: E,
    pub relations: R,
}

pub trait BaseRepository: Clone + Default + Sized {
    type Client<'a>: Copy;
    type Error: RepositoryError
        + From<std::sync::PoisonError<std::sync::RwLockReadGuard<'static, ()>>>
        + From<std::sync::PoisonError<std::sync::RwLockWriteGuard<'static, ()>>>;

    fn read() -> std::sync::LockResult<std::sync::RwLockReadGuard<'static, ()>>;
    fn write() -> std::sync::LockResult<std::sync::RwLockWriteGuard<'static, ()>>;
}

pub trait RepositoryError {
    fn not_found() -> Self;
}

#[derive(Clone, Debug)]
pub enum Cardinality {
    One,
    OneOrNone,
    AtLeastOne,
    Many,
}

pub struct One {}
pub struct OneOrNone {}
pub struct AtLeastOne {}
pub struct Many {}

impl From<One> for Cardinality {
    fn from(_: One) -> Cardinality {
        Cardinality::One
    }
}

impl From<OneOrNone> for Cardinality {
    fn from(_: OneOrNone) -> Cardinality {
        Cardinality::OneOrNone
    }
}

impl From<AtLeastOne> for Cardinality {
    fn from(_: AtLeastOne) -> Cardinality {
        Cardinality::AtLeastOne
    }
}

impl From<Many> for Cardinality {
    fn from(_: Many) -> Cardinality {
        Cardinality::Many
    }
}

pub trait Transactional {
    type AdaptorError;

    fn with_transaction<T, E, F>(&self, f: F) -> Result<T, E>
    where
        F: FnOnce() -> Result<T, E>,
        E: From<Self::AdaptorError>;
}
