#![feature(generic_associated_types)]

use itertools::izip;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::{Map, Zip};
use std::vec::IntoIter;

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

impl<E, R> From<(E, R)> for Entity<E, R> {
    fn from((value, relations): (E, R)) -> Self {
        Entity {
            value,
            relations,
        }
    }
}

pub trait BaseRepository: Clone + Default + Sized {
    type Client<'a>: Copy;
    type Error: std::fmt::Debug
        + Eq
        + RepositoryError
        + From<std::sync::PoisonError<std::sync::RwLockReadGuard<'static, ()>>>
        + From<std::sync::PoisonError<std::sync::RwLockWriteGuard<'static, ()>>>;

    fn read() -> std::sync::LockResult<std::sync::RwLockReadGuard<'static, ()>>;
    fn write() -> std::sync::LockResult<std::sync::RwLockWriteGuard<'static, ()>>;
}

pub trait RepositoryError {
    fn not_found() -> Self;
}

pub trait Transactional {
    type AdaptorError;

    fn with_transaction<T, E, F>(&self, f: F) -> Result<T, E>
    where
        F: FnOnce() -> Result<T, E>,
        E: From<Self::AdaptorError>;
}

pub fn dupe<T: Clone>((value, count): (T, usize)) -> Vec<T> {
    if count == 0 {
        return vec![]
    }
    let mut dupes: Vec<T> = Vec::with_capacity(count);
    for _ in 0..count - 1 {
        dupes.push(value.clone());
    }
    dupes.push(value);
    dupes
}

pub fn dupe_iter<T: Clone>((value, count): (T, usize)) -> DupeIter<T> {
    DupeIter { value: Some(value), count }
}

pub struct DupeIter<T> {
    value: Option<T>,
    count: usize,
}

impl<T: Clone> Iterator for DupeIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.count > 1 {
            Some(self.value.as_ref().unwrap().clone())
        } else if self.count == 1 {
            Some(std::mem::take(&mut self.value).unwrap())
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct PreSortValues<T> {
    unique_pre_sort_values: Vec<T>,
    unique_pre_sort_value_counts: Option<Vec<usize>>,
    sort_order: Option<Vec<Option<usize>>>,
}

#[derive(Debug)]
pub struct PreSortValuesIter<I> {
    unique_pre_sort_values_iter: I,
    unique_pre_sort_value_counts: Option<Vec<usize>>,
    sort_order: Option<Vec<Option<usize>>>,
}

impl<T> From<Vec<T>> for PreSortValues<T> {
    fn from(values: Vec<T>) -> Self {
        PreSortValues {
            unique_pre_sort_values: values,
            unique_pre_sort_value_counts: None,
            sort_order: None,
        }
    }
}

impl<T: std::fmt::Debug> PreSortValues<T> {
    /// Assumes unique_pre_sort_values only contains unique values but handles the case in which
    /// the map as_key is not injective (i.e. there are multiple unique values with the same key).
    pub fn from_unique_values_and_keys<K: std::fmt::Debug + Eq + Hash>(unique_pre_sort_values: Vec<T>, keys: &Vec<K>, as_key: impl Fn(&T) -> &K) -> Self {
        let mut key_map: HashMap<&K, Vec<usize>> = HashMap::with_capacity(unique_pre_sort_values.len());
        for (index, value) in unique_pre_sort_values.iter().enumerate() {
            let key = as_key(&value);
            if key_map.contains_key(key) {
                key_map.get_mut(key).unwrap().push(index);
            } else {
                key_map.insert(key, vec![index]);
            }
        }

        let mut unique_pre_sort_value_counts = vec![0_usize; unique_pre_sort_values.len()];
        let mut sort_order: Vec<Option<usize>> = Vec::with_capacity(keys.len());
        for key in keys.iter() {
            if let Some(indices) = key_map.get(key) {
                for index in indices.iter() {
                    unique_pre_sort_value_counts[*index] += 1;
                    sort_order.push(Some(*index));
                }
            } else {
                sort_order.push(None);
            }
        }

        PreSortValues {
            unique_pre_sort_values,
            unique_pre_sort_value_counts: Some(unique_pre_sort_value_counts),
            sort_order: Some(sort_order),
        }
    }

    pub fn from_unique_values_and_key_options<K: std::fmt::Debug + Eq + Hash>(unique_pre_sort_values: Vec<T>, keys: &Vec<Option<K>>, as_key: impl Fn(&T) -> &K) -> Self {
        let mut key_map: HashMap<&K, Vec<usize>> = HashMap::with_capacity(unique_pre_sort_values.len());
        for (index, value) in unique_pre_sort_values.iter().enumerate() {
            let key = as_key(&value);
            if key_map.contains_key(key) {
                key_map.get_mut(key).unwrap().push(index);
            } else {
                key_map.insert(key, vec![index]);
            }
        }

        let mut unique_pre_sort_value_counts = vec![0_usize; unique_pre_sort_values.len()];
        let mut sort_order: Vec<Option<usize>> = Vec::with_capacity(keys.len());
        for key in keys.iter() {
            if let Some(key) = key.as_ref() {
                if let Some(indices) = key_map.get(key) {
                    for index in indices.iter() {
                        unique_pre_sort_value_counts[*index] += 1;
                        sort_order.push(Some(*index));
                    }
                } else {
                    sort_order.push(None);
                }
            } else {
                sort_order.push(None);
            }
        }

        PreSortValues {
            unique_pre_sort_values,
            unique_pre_sort_value_counts: Some(unique_pre_sort_value_counts),
            sort_order: Some(sort_order),
        }
    }

    pub fn values(self) -> Vec<T> {
        self.unique_pre_sort_values
    }

    pub fn len(&self) -> usize {
        if let Some(sort_order) = self.sort_order.as_ref() {
            sort_order.iter().filter_map(|x| x.as_ref()).collect::<Vec<_>>().len()
        } else {
            self.unique_pre_sort_values.len()
        }
    }

    pub fn map_and_take<U, V, E: RepositoryError>(self, f: impl FnOnce(Vec<T>) -> (Vec<U>, V)) -> Result<(PreSortValues<U>, V), E> {
        if let Some(sort_order) = self.sort_order.as_ref() {
            if sort_order.iter().any(|index| index.is_none()) {
                return Err(E::not_found())
            }
        }

        let (unique_pre_sort_values, v) = f(self.unique_pre_sort_values);
        Ok((
            PreSortValues {
                unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
                sort_order: self.sort_order,
                unique_pre_sort_values,
            },
            v,
        ))
    }

    pub fn try_map_and_take<U, V>(self, f: impl FnOnce(Vec<T>) -> (Vec<U>, V)) -> (PreSortValues<U>, V) {
        let (unique_pre_sort_values, v) = f(self.unique_pre_sort_values);
        (
            PreSortValues {
                unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
                sort_order: self.sort_order,
                unique_pre_sort_values,
            },
            v,
        )
    }

    pub fn into_iter(self) -> PreSortValuesIter<IntoIter<T>> {
        PreSortValuesIter {
            unique_pre_sort_values_iter: self.unique_pre_sort_values.into_iter(),
            unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
            sort_order: self.sort_order,
        }
    }
}

impl<T, U> PreSortValues<(T, U)> {
    pub fn take_right(self) -> (PreSortValues<T>, Vec<U>) {
        let (unique_pre_sort_values, right_values): (Vec<_>, Vec<_>) = self.unique_pre_sort_values.into_iter().unzip();
        (
            PreSortValues {
                unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
                sort_order: self.sort_order,
                unique_pre_sort_values,
            },
            right_values,
        )
    }
}

impl<T: std::fmt::Debug, I: Iterator<Item = T>> PreSortValuesIter<I> {
    pub fn collect(self) -> PreSortValues<T> {
        PreSortValues {
            unique_pre_sort_values: self.unique_pre_sort_values_iter.collect(),
            unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
            sort_order: self.sort_order,
        }
    }

    pub fn map<U, F: FnMut(T) -> U>(self, f: F) -> PreSortValuesIter<Map<I, F>> {
        PreSortValuesIter {
            unique_pre_sort_values_iter: self.unique_pre_sort_values_iter.map(f),
            unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
            sort_order: self.sort_order,
        }
    }

    pub fn map_and_take<U, V, E: RepositoryError>(self, f: impl FnOnce(Vec<T>) -> (Vec<U>, V)) -> Result<(PreSortValues<U>, V), E> {
        self.collect().map_and_take(f)
    }

    pub fn try_map_and_take<U, V>(self, f: impl FnOnce(Vec<T>) -> (Vec<U>, V)) -> (PreSortValues<U>, V) {
        self.collect().try_map_and_take(f)
    }

    pub fn zip<U>(self, values: Vec<U>) -> PreSortValuesIter<Zip<I, IntoIter<U>>> {
        PreSortValuesIter {
            unique_pre_sort_values_iter: izip!(self.unique_pre_sort_values_iter, values.into_iter()),
            unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
            sort_order: self.sort_order,
        }
    }

    pub fn zip_with<U, V, F: Fn((T, U)) -> V>(self, values: Vec<U>, f: F) -> PreSortValuesIter<Map<Zip<I, IntoIter<U>>, F>> {
        PreSortValuesIter {
            unique_pre_sort_values_iter: izip!(self.unique_pre_sort_values_iter, values.into_iter()).map(f),
            unique_pre_sort_value_counts: self.unique_pre_sort_value_counts,
            sort_order: self.sort_order,
        }
    }

    pub fn dupe_and_sort<E: RepositoryError>(self) -> Result<Vec<T>, E>
    where
        T: Clone,
    {
        if let Some(unique_pre_sort_value_counts) = self.unique_pre_sort_value_counts {
            if let Some(sort_order) = self.sort_order {
                let mut duped_unique_pre_sort_values: Vec<Vec<T>> = izip!(
                    self.unique_pre_sort_values_iter,
                    unique_pre_sort_value_counts.into_iter(),
                )
                    .map(dupe)
                    .collect();

                return sort_order
                    .into_iter()
                    .map(|index|
                        index
                            .map(|index| duped_unique_pre_sort_values[index].pop().unwrap())
                            .ok_or_else(E::not_found)
                    )
                    .collect();
            }
        }
        Ok(self.unique_pre_sort_values_iter.collect())
    }

    pub fn try_dupe_and_sort(self) -> Vec<Option<T>>
    where
        T: Clone,
    {
        if let Some(unique_pre_sort_value_counts) = self.unique_pre_sort_value_counts {
            if let Some(sort_order) = self.sort_order {
                let mut duped_unique_pre_sort_values: Vec<Vec<T>> = izip!(
                    self.unique_pre_sort_values_iter,
                    unique_pre_sort_value_counts.into_iter(),
                )
                    .map(dupe)
                    .collect();

                return sort_order
                    .into_iter()
                    .map(|index| index.map(|index| duped_unique_pre_sort_values[index].pop().unwrap()))
                    .collect();
            }
        }
        self.unique_pre_sort_values_iter.map(Option::from).collect()
    }
}
