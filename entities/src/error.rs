use repositories::RepositoryError;
use std::sync::PoisonError;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EntityError {
    ForeignKeyViolation {
        message: String,
        details: Option<String>,
        hint: Option<String>,
        table_name: Option<String>,
        column_name: Option<String>,
        constraint_name: Option<String>,
    },
    NotFound,
    Other {
        message: String,
        details: Option<String>,
        hint: Option<String>,
        table_name: Option<String>,
        column_name: Option<String>,
        constraint_name: Option<String>,
    },
    Poison,
    UniqueViolation {
        message: String,
        details: Option<String>,
        hint: Option<String>,
        table_name: Option<String>,
        column_name: Option<String>,
        constraint_name: Option<String>,
    },
}

impl RepositoryError for EntityError {
    fn not_found() -> Self {
        EntityError::NotFound
    }
}

impl<T> From<PoisonError<T>> for EntityError {
    fn from(_: PoisonError<T>) -> EntityError {
        EntityError::Poison
    }
}

cfg_if! {
    if #[cfg(feature = "diesel")] {
        use opt_diesel::result::{Error as DieselError, DatabaseErrorKind};
        impl From<DieselError> for EntityError {
            fn from(err: DieselError) -> EntityError {
                match err {
                    DieselError::DatabaseError(kind, information) => {
                        let message = String::from(information.message());
                        let details = information.details().map(|x| String::from(x));
                        let hint = information.hint().map(|x| String::from(x));
                        let table_name = information.table_name().map(|x| String::from(x));
                        let column_name = information.column_name().map(|x| String::from(x));
                        let constraint_name = information.constraint_name().map(|x| String::from(x));

                        match kind {
                            DatabaseErrorKind::UniqueViolation => EntityError::UniqueViolation { message, details, hint, table_name, column_name, constraint_name },
                            DatabaseErrorKind::ForeignKeyViolation => EntityError::ForeignKeyViolation { message, details, hint, table_name, column_name, constraint_name },
                            _ => EntityError::Other { message, details, hint, table_name, column_name, constraint_name },
                        }
                    },
                    DieselError::NotFound => EntityError::NotFound,
                    err => EntityError::Other {
                        message: format!("{}", err),
                        details: None,
                        hint: None,
                        table_name: None,
                        column_name: None,
                        constraint_name: None,
                    },
                }
            }
        }
    }
}

#[macro_export]
macro_rules! error {
    ($enum_ty:ident { $(
        $(#[$meta:meta])*
        $variant:ident$(($err_ty:ty))?
    ),* $(,)? }) => {

        #[derive(Clone, Debug, Deserialize, Serialize)]
        #[serde(tag = "type")]
        pub enum $enum_ty {
            $(
                $(#[$meta])*
                $variant$(($err_ty))?,
            )*
        }

        $( $(
            impl From<$err_ty> for $enum_ty {
                fn from(err: $err_ty) -> Self {
                    $enum_ty::$variant(err)
                }
            }
        )? )*

    };
}
