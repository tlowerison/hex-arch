pub use paste::paste as fields_paste;

#[macro_export]
macro_rules! maybe_option {
    (!, $ty:ty) => { $ty };
    (?, $ty:ty) => { Option<$ty> };
}

#[macro_export]
macro_rules! maybe_some {
    (!, $expr:expr) => {
        $expr
    };
    (?, $expr:expr) => {
        Some($expr)
    };
}

#[macro_export]
macro_rules! maybe_unreachable {
    (!) => {
        #[allow(unreachable_code)]
        unreachable!()
    };
    (?) => {
        None
    };
}

#[macro_export]
macro_rules! unwrap_helper_field {
    (!, $ident:ident, $helper_ident:ident, $field_ident:ident) => {
        $helper_ident.$field_ident.ok_or_else(|| {
            syn::Error::new(
                $ident.span(),
                concat!("missing required field `", stringify!($field_ident), "`"),
            )
        })?
    };
    (?, $ident:ident, $helper_ident:ident, $field_ident:ident) => {
        $helper_ident.$field_ident.unwrap_or(None)
    };
}

#[macro_export]
macro_rules! fields {
    (
        $fields_ident:ident {
            $(
                $field_ident:ident$opt:tt: $input_ident:ident -> $ty:ty $block:block
            ),*$(,)?
        }
    ) => {
        fields_paste! {
            pub (crate) enum [<$fields_ident Field>] {
                $(
                    #[allow(non_camel_case_types)]
                    [<$field_ident:upper>]((Ident, $ty))
                ),*
            }

            pub (crate) struct [<$fields_ident Fields>] {
                $($field_ident: $crate::maybe_option!($opt, $ty)),*
            }

            #[allow(non_camel_case_types)]
            #[derive(Default)]
            struct [<$fields_ident FieldsHelper>] {
                $($field_ident: Option<$crate::maybe_option!($opt, $ty)>),*
            }

            impl TryFrom<(Ident, [<$fields_ident FieldsHelper>])> for [<$fields_ident Fields>] {
                type Error = syn::Error;

                fn try_from((ident, helper): (Ident, [<$fields_ident FieldsHelper>])) -> syn::Result<Self> {
                    Ok([<$fields_ident Fields>] {
                        $($field_ident: unwrap_helper_field!($opt, ident, helper, $field_ident)),*
                    })
                }
            }

            impl Eq for [<$fields_ident Field>] {}

            impl PartialEq for [<$fields_ident Field>] {
                fn eq(&self, other: &Self) -> bool {
                    match self {
                        $(
                            [<$fields_ident Field>]::[<$field_ident:upper>](_) => match other {
                                [<$fields_ident Field>]::[<$field_ident:upper>](_) => true,
                                _ => false,
                            }
                        ),*
                    }
                }
            }

            impl syn::parse::Parse for [<$fields_ident Field>] {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    let ident: Ident = input.parse()?;
                    let _colon: Token![:] = input.parse()?;

                    match &*format!("{}", ident) {
                        $(stringify!($field_ident) => {
                            let $input_ident = input;
                            Ok([<$fields_ident Field>]::[<$field_ident:upper>]((ident, $block)))
                        }),*
                        other => Err(syn::Error::new(
                            ident.span(),
                            format!(concat!("expected ", $(concat!("`"), stringify!($field_ident), concat!("`,"),)* " found `{}`"), other)
                        )),
                    }
                }
            }

            impl syn::parse::Parse for [<$fields_ident Fields>] {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    let mut fields_helper = [<$fields_ident FieldsHelper>]::default();
                    if input.is_empty() {
                        #[allow(unreachable_code)]
                        return Ok([<$fields_ident Fields>] {
                            $(
                                $field_ident: { $crate::maybe_unreachable!($opt) }
                            ),*
                        });
                    }

                    let last_ident = loop {
                        let field: [<$fields_ident Field>] = input.parse()?;
                        let ident = match field {
                            $([<$fields_ident Field>]::[<$field_ident:upper>]((ident, field)) => {
                                if let None = fields_helper.$field_ident {
                                    fields_helper.$field_ident = Some($crate::maybe_some!($opt, field));
                                    ident
                                } else {
                                    return Err(syn::Error::new(
                                        ident.span(),
                                        concat!("duplicate field `", stringify!($field_ident), "`"),
                                    ));
                                }
                            }),*
                        };
                        let comma: Option<syn::Token![,]> = input.parse().ok();
                        if let None = comma {
                            break ident;
                        }
                        if input.is_empty() {
                            break ident;
                        }
                    };

                    (last_ident, fields_helper).try_into()
                }
            }
        }
    };
}
