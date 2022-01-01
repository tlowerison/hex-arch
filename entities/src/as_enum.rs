#[macro_export]
macro_rules! as_enum {
    (
        $(#[$attr:meta])*
        $vis:vis enum $ident:ident ($ty:ty) {
            $(
                $(#[$sub_attr:meta])*
                $branch:ident = $id:literal
            ),*$(,)?
        }
    ) => {
        entities_paste! {
            $(#[$attr])*
            $vis enum $ident {
                $(
                    $(#[$sub_attr])*
                    $branch = $id,
                )*
            }

            $(
                pub const [<$ident:upper _ $branch:upper _ID>]: $ty = $id;
            )*

            impl AsRef<$ty> for $ident {
                fn as_ref(&self) -> &$ty {
                    match self {
                        $($ident::$branch => &[<$ident:upper _ $branch:upper _ID>],)*
                    }
                }
            }

            impl Into<$ty> for &$ident {
                fn into(self) -> $ty {
                    self.as_ref().clone()
                }
            }

            impl Into<$ty> for $ident {
                fn into(self) -> $ty {
                    (&self).into()
                }
            }

            impl TryFrom<$ty> for $ident {
                type Error = [<$ident Error>];

                fn try_from(id: $ty) -> Result<Self, Self::Error> {
                    match id {
                        $([<$ident:upper _ $branch:upper _ID>] => Ok($ident::$branch),)*
                        _ => Err([<$ident Error>]::invalid(id)),
                    }
                }
            }
        }
    };
}
