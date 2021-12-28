pub use common::transpose_2 as data_sources_transpose_2;
pub use proc_macros::*;

pub trait IsChangeset {
    fn is_changeset(&self) -> bool;
}
