#[macro_export]
macro_rules! dependent_fields {
    ($(
        $error_ty:ty;
        $(#[$attr:meta])*
        $vis:vis struct $ident:ident {
            $(
                $(#[$field_attr:meta])*
                $field_vis:vis $field_ident:ident: $field_ty:ty
            ),*$(,)?
        }
    )*) => {
        entities_paste! {
            $(
                $(#[$attr])*
                $vis struct $ident {
                    $(
                        $(#[$field_attr])*
                        $field_vis $field_ident: $field_ty
                    ),*
                }

                impl $ident {
                    pub fn try_opt_from($($field_ident: Option<$field_ty>,)*) -> Result<Option<Self>, $error_ty> {
                        dependent_fields!(@ $ident {} $($field_ident)* | $error_ty);
                        Ok(None)
                    }
                }
            )*
        }
    };

    (@ $ident:ident { $($tt:tt)* } $field_ident_1:ident $($field_ident:ident)* | $error_ty:ty) => {
        entities_paste! {
            if let Some($field_ident_1) = $field_ident_1 {
                dependent_fields!(@ $ident { $($tt)* $field_ident_1 } $($field_ident)* | $error_ty);
            } $( else if let Some(_) = $field_ident {
                return Err($error_ty::[<_requires_ $field_ident_1>]());
            })*
        }
    };

    (@ $ident:ident { $($field_ident:ident)* } | $error_ty:ty) => {
        return Ok(Some($ident { $( $field_ident ),* }));
    };
}

// Ex:
//
// dependent_fields!(
//     FooError;
//     #[derive(Clone, Debug)]
//     pub struct Foo {
//         pub a: A,
//         pub b: Vec<B>,
//         pub c: Option<C>,
//     }
// );
//
// ----- expands to -----
//
// #[derive(Clone, Debug)]
// pub struct Foo {
//     pub a: A,
//     pub b: Vec<B>,
//     pub c: Option<C>,
// }
//
// impl Foo {
//     pub fn try_opt_from(
//         a: Option<A>,
//         b: Option<Vec<B>>,
//         c: Option<Option<C>>,
//     ) -> Result<Option<Self>, FooError> {
//         if let Some(a) = a {
//             if let Some(b) = b {
//                 if let Some(c) = c {
//                     return Ok(Some(Foo { a, b, c }));
//                 }
//             } else if let Some(_) = b {
//                 return Err(FooError::_requires_b());
//             }
//         } else if let Some(_) = b {
//             return Err(FooError::_requires_a());
//         } else if let Some(_) = c {
//             return Err(FooError::_requires_a());
//         }
//         Ok(None)
//     }
// }
