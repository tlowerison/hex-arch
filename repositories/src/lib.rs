#![feature(generic_associated_types)]

pub use proc_macros::*;

#[derive(Clone, Debug)]
pub struct Entity<E, R> {
    pub value: E,
    pub relations: R,
}

impl<K, E: AsRef<K>, R> AsRef<K> for Entity<std::sync::Arc<E>, R> {
    fn as_ref(&self) -> &K {
        self.value.as_ref().as_ref()
    }
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

pub fn dupe<C: Clone>(c: C, count: usize) -> Vec<C> {
    if count == 0 {
        return vec![]
    }
    let mut dupes: Vec<C> = Vec::with_capacity(count);
    for _ in 0..count - 1 {
        dupes.push(c.clone());
    }
    dupes.push(c);
    dupes
}

pub fn dupe_iter<C: Clone>(c: C, count: usize) -> DupeIter<C> {
    DupeIter { c: Some(c), count }
}

pub struct DupeIter<C> {
    c: Option<C>,
    count: usize,
}

impl<C: Clone> Iterator for DupeIter<C> {
    type Item = C;
    fn next(&mut self) -> Option<Self::Item> {
        if self.count > 1 {
            Some(self.c.as_ref().unwrap().clone())
        } else if self.count == 1 {
            Some(std::mem::take(&mut self.c).unwrap())
        } else {
            None
        }
    }
}
