use convert_case::{Case, Casing};
use proc_macro2::TokenStream as TokenStream2;
use syn::{braced, Ident, parenthesized, token::{Brace, Paren}, Token};
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
    pub (crate) repositories: Vec<RepositoryInput>,
}

pub (crate) struct EntityIdents {
    pub (crate) ty: Ident,
    pub (crate) singular: Ident,
    pub (crate) plural: Ident,
}

pub (crate) struct RepositoryInput {
    pub (crate) idents: EntityIdents,
    pub (crate) id_type: Punctuated<Ident, Token![::]>,
    pub (crate) mutability: Mutability,
    pub (crate) relations: Vec<RelationInput>,
    syncability: Option<Syncability>,
}

pub (crate) struct RelationEntityIdents {
    pub (crate) ty: Ident,
    pub (crate) snake: Ident,
    pub (crate) plural: Ident,
}

pub (crate) struct RelationInput {
    pub (crate) idents: RelationEntityIdents,
    pub (crate) cardinality: Cardinality,
    syncability: Option<Syncability>,
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
        let in_braces;

        let syncability: Syncability = Syncability::from(&input.parse()?);
        let _brace: Brace = braced!(in_braces in input);
        let repositories: Punctuated<RepositoryInput, Token![,]> = in_braces.parse_terminated(RepositoryInput::parse)?;
        Ok(RepositoriesInput {
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
        })
    }
}

impl Parse for RepositoryInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_braces;
        let in_parens;

        let ty: Ident = input.parse()?;

        let _paren: Paren = parenthesized!(in_parens in input);
        let id_type: Punctuated<Ident, Token![::]> = in_parens.parse_terminated(Ident::parse)?;

        let idents: EntityIdents = EntityIdents {
            singular: format_ident!("{}", format!("{}", ty).to_case(Case::Snake)),
            plural: if input.peek(Paren) {
                let in_parens;
                let _paren: Paren = parenthesized!(in_parens in input);
                in_parens.parse()?
            } else {
                format_ident!("{}", format!("{}s", ty).to_case(Case::Snake))
            },
            ty,
        };

        let mutability: Mutability = Mutability::from(&input.parse()?);
        let _brace: Brace = braced!(in_braces in input);
        let relations: Punctuated<RelationInput, Token![,]> = in_braces.parse_terminated(RelationInput::parse)?;
        Ok(RepositoryInput {
            idents,
            id_type,
            mutability,
            relations: relations.into_iter().collect(),
            syncability: None,
        })
    }
}

impl Parse for RelationInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let in_parens;

        let snake_ident: Ident = input.parse()?;
        let plural_ident: Option<Ident> = if input.peek(Paren) {
            let in_parens;
            let _paren: Paren = parenthesized!(in_parens in input);
            Some(in_parens.parse()?)
        } else {
            None
        };

        let _colon: Token![:] = input.parse()?;
        let _paren: Paren = parenthesized!(in_parens in input);
        let ty_ident: Ident = in_parens.parse()?;
        let _comma: Token![,] = in_parens.parse()?;
        let cardinality = Cardinality::from(&in_parens.parse()?);

        Ok(RelationInput {
            syncability: None,
            idents: RelationEntityIdents {
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

    pub (crate) fn write_tys(&self) -> Vec<&Ident> {
        self.repositories
            .iter()
            .filter_map(|repository| if let Mutability::RW = repository.mutability { Some(repository.ty()) } else { None })
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

impl Syncability {
    pub (crate) fn sync_ptr(&self) -> TokenStream2 {
        match &self {
            Syncability::Sync => "std::sync::Arc".parse().unwrap(),
            Syncability::Unsync => "std::rc::Rc".parse().unwrap(),
        }
    }
}
