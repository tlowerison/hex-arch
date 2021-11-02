pub use common::transpose_2 as repositories_transpose_2;
pub use itertools::{izip as repositories_izip, Itertools as RepositoriesItertools};
pub use paste::paste as repositories_paste;
pub use proc_macros::*;

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
