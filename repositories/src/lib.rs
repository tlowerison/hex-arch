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

pub use common::transpose_2;

#[macro_export]
macro_rules! repositories {
    (
        sync {
            $(
                $entity:ident $mutability:ident {
                    $(
                        $singular_foreign_field_name:ident: ($related_entity_name:ident, $relation_cardinality:ident)
                    ),*$(,)?
                }
            ),*$(,)?
        }
    ) => {
        repositories_paste! {
            proc_repositories! {
                sync |
                $(
                    $entity,
                    $mutability,
                    $(
                        $singular_foreign_field_name,
                        $related_entity_name,
                        $relation_cardinality,
                    )*
                )|*
            }
        }
    };

    (
        unsync {
            $(
                $entity:ident $mutability:ident {
                    $(
                        $singular_foreign_field_name:ident: ($related_entity_name:ident, $relation_cardinality:ident),
                    )*
                },
            )*
        }
    ) => {
        repositories_paste! {
            proc_repositories! {
                unsync |
                $(
                    $entity,
                    $mutability,
                    $(
                        $singular_foreign_field_name,
                        $related_entity_name,
                        $relation_cardinality,
                    )*
                )|*
            }
        }
    };
}

#[macro_export]
macro_rules! print_repositories {
    (
        sync {
            $(
                $entity:ident $mutability:ident {
                    $(
                        $singular_foreign_field_name:ident: ($related_entity_name:ident, $relation_cardinality:ident)
                    ),*$(,)?
                }
            ),*$(,)?
        }
    ) => {
        repositories_paste! {
            proc_print_repositories! {
                sync |
                $(
                    $entity,
                    $mutability,
                    $(
                        $singular_foreign_field_name,
                        $related_entity_name,
                        $relation_cardinality,
                    )*
                )|*
            }
        }
    };

    (
        unsync {
            $(
                $entity:ident $mutability:ident {
                    $(
                        $singular_foreign_field_name:ident: ($related_entity_name:ident, $relation_cardinality:ident),
                    )*
                },
            )*
        }
    ) => {
        repositories_paste! {
            proc_print_repositories! {
                unsync |
                $(
                    $entity,
                    $mutability,
                    $(
                        $singular_foreign_field_name,
                        $related_entity_name,
                        $relation_cardinality,
                    )*
                )|*
            }
        }
    };
}
