use proc_macro2::TokenStream;
use syn::{parse::{Parse, ParseStream}, punctuated::Punctuated, token, Ident, Lifetime, Path};
use quote::ToTokens;

pub struct BorrowAttr {
    pub access: Punctuated<FieldAccess, token::Comma>,
    pub path: Path,
}

pub enum FieldAccess {
    Field(Option<Lifetime>, bool, Ident),
    Star(Option<Lifetime>, bool),
    Excl(Ident),
}

impl ToTokens for FieldAccess {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FieldAccess::Field(lt, mu, ident) => {
                if let Some(lt) = lt { lt.to_tokens(tokens) };
                if *mu { token::Mut::default().to_tokens(tokens); }
                ident.to_tokens(tokens);
            },
            FieldAccess::Star(lt, mu) => {
                if let Some(lt) = lt { lt.to_tokens(tokens) };
                if *mu { token::Mut::default().to_tokens(tokens) };
                token::Star::default().to_tokens(tokens);
            },
            FieldAccess::Excl(ident) => {
                token::Not::default().to_tokens(tokens);
                ident.to_tokens(tokens);
            },
        }
    }
}

impl Parse for FieldAccess {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if input.peek(token::Not) {
            input.parse::<token::Not>()?;
            return Ok(FieldAccess::Excl(input.parse()?));
        }
        let lt: Option<Lifetime> = if input.peek(Lifetime) { Some(input.parse()?) } else { None };
        let mu = input.peek(token::Mut);
        if mu { input.parse::<token::Mut>()?; };
        Ok(if input.peek(token::Star) {
            input.parse::<token::Star>()?;
            FieldAccess::Star(lt, mu)
        } else {
            FieldAccess::Field(lt, mu, input.parse()?)
        })
    }
}

impl Parse for BorrowAttr {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        input.parse::<token::And>()?;
        input.parse::<token::Lt>()?;
        Ok(BorrowAttr {
            access: {
                let mut access = Punctuated::new();
                while !input.peek(token::Gt) {
                    access.push_value(input.parse()?);
                    if input.peek(token::Gt) { input.parse::<token::Gt>()?; break }
                    access.push_punct(input.parse()?);
                }
                access
            },
            path: input.parse()?,
        })
    }
}
impl ToTokens for BorrowAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        token::And::default().to_tokens(tokens);
        token::Lt::default().to_tokens(tokens);
        self.access.to_tokens(tokens);
        token::Gt::default().to_tokens(tokens);
        self.path.to_tokens(tokens);
    }
}
