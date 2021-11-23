use convert_case::{Case, Casing};
use proc_macro2::TokenStream as TokenStream2;
use syn::{braced, Ident, parenthesized, token::Paren, Token, Type};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

#[derive(Clone, Debug)]
pub (crate) enum Cardinality {
    One,
    OneOrNone,
    AtLeastOne,
    Many,
}

#[derive(Clone, Debug)]
pub (crate) enum Mutability {
    R,
    RW,
}

#[derive(Clone, Debug)]
pub (crate) enum Syncability {
    Sync,
    Unsync,
}

pub (crate) struct RepositoriesInput {
    pub (crate) name: Option<Ident>,
    pub (crate) repositories: Vec<RepositoryInput>,
}

pub (crate) struct RepositoryIdents {
    pub (crate) ty: Ident,
    pub (crate) singular: Ident,
    pub (crate) plural: Ident,
}

pub (crate) struct RepositoryInput {
    pub (crate) idents: RepositoryIdents,
    pub (crate) id_type: Punctuated<Ident, Token![::]>,
    pub (crate) mutability: Mutability,
    pub (crate) read_repositories: TokenStream2,
    pub (crate) relations: Vec<RelationInput>,
    pub (crate) load_bys: Vec<LoadByInput>,
    syncability: Option<Syncability>,
}

pub (crate) struct RelationIdents {
    pub (crate) ty: Ident,
    pub (crate) snake: Ident,
    pub (crate) plural: Ident,
}

pub (crate) struct RelationInput {
    pub (crate) idents: RelationIdents,
    pub (crate) cardinality: Cardinality,
    syncability: Option<Syncability>,
}

pub (crate) struct LoadByIdents {
    pub (crate) singular: Ident,
    pub (crate) plural: Ident,
}

pub (crate) struct LoadByInput {
    pub (crate) idents: LoadByIdents,
    pub (crate) ty: Type,
    pub (crate) cardinality: Cardinality,
}

optional_fields! {
    RepositoryInput {
        relations!: input -> Vec<RelationInput> {
            let in_brace;
            braced!(in_brace in input);
            let relations: Punctuated<RelationInput, Token![,]> = in_brace.parse_terminated(RelationInput::parse)?;
            relations.into_iter().collect()
        },
        load_by?: input -> Vec<LoadByInput> {
            let in_brace;
            braced!(in_brace in input);
            let load_bys: Punctuated<LoadByInput, Token![,]> = in_brace.parse_terminated(LoadByInput::parse)?;
            load_bys.into_iter().collect()
        },
    }
}


impl From<&Ident> for Cardinality {
    fn from(ident: &Ident) -> Cardinality {
        match &*format!("{}", ident) {
            "One" => Cardinality::One,
            "OneOrNone" => Cardinality::OneOrNone,
            "AtLeastOne" => Cardinality::AtLeastOne,
            "Many" => Cardinality::Many,
            other => panic!("unrecognized relation cardinality: expected one of {{One, OneOrNone, Many, AtLeastOne}}, received {}", other),
        }
    }
}

impl From<&Ident> for Mutability {
    fn from(ident: &Ident) -> Mutability {
        let string = format!("{}", ident);
        match &*string {
            "R" => Mutability::R,
            "RW" => Mutability::RW,
            other => panic!("unrecognized mutability: expected one of {{R, RW}}, received {}", other),
        }
    }
}

impl Parse for RepositoriesInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_brace;

        let syncability: Syncability = Syncability::from(&input.parse()?);
        let name: Option<Ident> = input.parse().ok();

        braced!(in_brace in input);
        let repositories: Punctuated<RepositoryInput, Token![,]> = in_brace.parse_terminated(RepositoryInput::parse)?;

        let mut input = RepositoriesInput {
            name,
            repositories: repositories
                .into_iter()
                .map(|repository| RepositoryInput {
                    syncability: Some(syncability.clone()),
                    relations: repository.relations
                        .into_iter()
                        .map(|relation| RelationInput {
                            syncability: Some(syncability.clone()),
                            ..relation
                        })
                        .collect(),
                    ..repository
                })
                .collect(),
        };

        let read_repositories = input.tys().into_iter().map(|ty| format!("{}ReadRepository", ty)).collect::<Vec<_>>().join(" + ");

        for repository in input.repositories.iter_mut() {
            repository.read_repositories = read_repositories.clone().parse().unwrap();
        }

        Ok(input)
    }
}

impl Parse for RepositoryInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_brace;
        let in_paren;

        let ty: Ident = input.parse()?;

        parenthesized!(in_paren in input);
        let id_type: Punctuated<Ident, Token![::]> = in_paren.parse_terminated(Ident::parse)?;

        let idents: RepositoryIdents = RepositoryIdents {
            singular: format_ident!("{}", format!("{}", ty).to_case(Case::Snake)),
            plural: if input.peek(Paren) {
                let in_paren;
                let _paren: Paren = parenthesized!(in_paren in input);
                in_paren.parse()?
            } else {
                format_ident!("{}", format!("{}s", ty).to_case(Case::Snake))
            },
            ty,
        };

        let mutability: Mutability = Mutability::from(&input.parse()?);
        braced!(in_brace in input);

        let fields: RepositoryInputFields = in_brace.parse()?;

        Ok(RepositoryInput {
            idents,
            id_type,
            mutability,
            syncability: None,
            read_repositories: quote! {},
            load_bys: fields.load_by.unwrap_or(vec![]),
            relations: fields.relations,
        })
    }
}

impl Parse for RelationInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_paren;

        let snake_ident: Ident = input.parse()?;
        let plural_ident: Option<Ident> = if input.peek(Paren) {
            let in_paren;
            let _paren: Paren = parenthesized!(in_paren in input);
            Some(in_paren.parse()?)
        } else {
            None
        };

        let _colon: Token![:] = input.parse()?;
        let _paren: Paren = parenthesized!(in_paren in input);
        let ty_ident: Ident = in_paren.parse()?;
        let _comma: Token![,] = in_paren.parse()?;
        let cardinality = Cardinality::from(&in_paren.parse()?);

        Ok(RelationInput {
            syncability: None,
            idents: RelationIdents {
                ty: ty_ident,
                plural: plural_ident.unwrap_or_else(|| match cardinality {
                    Cardinality::One|Cardinality::OneOrNone => format_ident!("{}s", snake_ident),
                    _ => snake_ident.clone(),
                }),
                snake: snake_ident,
            },
            cardinality,
        })
    }
}

impl Parse for LoadByInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_paren;

        let singular: Ident = input.parse()?;
        let idents = LoadByIdents {
            plural: if input.peek(Paren) {
                let in_paren;
                parenthesized!(in_paren in input);
                in_paren.parse()?
            } else {
                format_ident!("{}", format!("{}s", singular))
            },
            singular,
        };

        let _colon: Token![:] = input.parse()?;
        parenthesized!(in_paren in input);

        let ty: Type = in_paren.parse()?;
        let _comma: Token![,] = in_paren.parse()?;

        let cardinality_span = in_paren.span();
        let cardinality = Cardinality::from(&in_paren.parse()?);

        match &cardinality {
            Cardinality::OneOrNone|Cardinality::AtLeastOne => return Err(syn::Error::new(cardinality_span, "load by cardinality must be one of {One, Many}")),
            _ => {},
        };

        Ok(LoadByInput {
            idents,
            ty,
            cardinality,
        })
    }
}

impl From<&Ident> for Syncability {
    fn from(ident: &Ident) -> Syncability {
        let string = format!("{}", ident);
        match &*string {
            "sync" => Syncability::Sync,
            "unsync" => Syncability::Unsync,
            other => panic!("unrecognized syncability: expected one of {{sync, unsync}}, received {}", other),
        }
    }
}

impl RepositoriesInput {
    pub (crate) fn tys(&self) -> Vec<&Ident> {
        self.repositories
            .iter()
            .map(|repository| repository.ty())
            .collect()
    }
}

impl RepositoryInput {
    pub (crate) fn ty(&self) -> &Ident {
        &self.idents.ty
    }

    pub (crate) fn singular(&self) -> &Ident {
        &self.idents.singular
    }

    pub (crate) fn plural(&self) -> &Ident {
        &self.idents.plural
    }

    pub (crate) fn relation_tys(&self) -> Vec<&Ident> {
        self.relations
            .iter()
            .map(|relation| relation.ty())
            .collect()
    }

    pub (crate) fn relation_snakes(&self) -> Vec<&Ident> {
        self.relations
            .iter()
            .map(|relation| relation.snake())
            .collect()
    }

    pub (crate) fn sync_ptr(&self) -> TokenStream2 {
        self.syncability.as_ref().unwrap().sync_ptr()
    }

    pub (crate) fn read_repositories(&self) -> &TokenStream2 {
        &self.read_repositories
    }
}

impl RelationInput {
    pub (crate) fn ty(&self) -> &Ident {
        &self.idents.ty
    }

    pub (crate) fn snake(&self) -> &Ident {
        &self.idents.snake
    }

    pub (crate) fn plural(&self) -> &Ident {
        &self.idents.plural
    }

    pub (crate) fn sync_ptr(&self) -> TokenStream2 {
        self.syncability.as_ref().unwrap().sync_ptr()
    }
}

impl LoadByInput {
    pub (crate) fn ty(&self) -> &Type {
        &self.ty
    }

    pub (crate) fn singular(&self) -> &Ident {
        &self.idents.singular
    }

    pub (crate) fn plural(&self) -> &Ident {
        &self.idents.plural
    }
}

impl Syncability {
    pub (crate) fn sync_ptr(&self) -> TokenStream2 {
        match &self {
            Syncability::Sync => "std::sync::Arc".parse().unwrap(),
            Syncability::Unsync => "std::rc::Rc".parse().unwrap(),
        }
    }
}
