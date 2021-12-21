pub use proc_macros::*;

pub trait IsChangeset {
    fn is_changeset(&self) -> bool;
}
