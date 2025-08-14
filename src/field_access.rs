use std::collections::BTreeMap;

use macro_magic::mm_core::ForeignPath;
use proc_macro2::TokenStream;
use syn::{parse::{Parse, ParseStream}, parse2, punctuated::Punctuated, token, Ident, Lifetime, Path, Type};
use quote::{ToTokens, quote};

pub struct CustomAttr {
    _amp: token::And,
    _langle: token::Lt,
    pub access: Punctuated<FieldAccess, token::Comma>,
    _rangle: token::Gt,
    pub path: Path,
}

pub enum FieldAccess {
    Field {
        lt: Option<Lifetime>,
        mut_tok: Option<token::Mut>,
        ident: Ident,
    },
    Star {
        lt: Option<Lifetime>,
        mut_tok: Option<token::Mut>,
        star: token::Star,
    },
    Excl {
        bang: token::Not,
        ident: Ident,
    },
}

impl ToTokens for FieldAccess {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FieldAccess::Field { lt, mut_tok, ident } => {
                if let Some(lt) = lt {
                    tokens.extend(lt.to_token_stream());
                }
                if let Some(mut_tok) = mut_tok {
                    tokens.extend(mut_tok.to_token_stream());
                }
                tokens.extend(ident.to_token_stream());
            },
            FieldAccess::Star { lt, mut_tok, star } => {
                if let Some(lt) = lt {
                    tokens.extend(lt.to_token_stream());
                }
                if let Some(mut_tok) = mut_tok {
                    tokens.extend(mut_tok.to_token_stream());
                }
                tokens.extend(star.to_token_stream());
            },
            FieldAccess::Excl { bang, ident } => {
                tokens.extend(bang.to_token_stream());
                tokens.extend(ident.to_token_stream());
            },
        }
    }
}

impl Parse for FieldAccess {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if input.peek(token::Not) {
            return Ok(FieldAccess::Excl {
                bang: input.parse()?,
                ident: input.parse()?,
            });
        }
        let lt: Option<Lifetime> = if input.peek(Lifetime) { Some(input.parse()?) } else { None };
        let mut_tok: Option<token::Mut> = if input.peek(token::Mut) { Some(input.parse()?) } else { None };
        return Ok(if input.peek(token::Star) {
            FieldAccess::Star { lt, mut_tok, star: input.parse()? }
        }
        else {
            FieldAccess::Field { lt, mut_tok, ident: input.parse()? }
        })
    }
}

impl Parse for CustomAttr {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        Ok(CustomAttr {
            _amp: input.parse()?,
            _langle: input.parse()?,
            // access: input.parse()?,
            access: {
                let mut access = Punctuated::new();
                while !input.peek(token::Gt) {
                    access.push_value(input.parse()?);
                    if input.peek(token::Gt) { break }
                    access.push_punct(input.parse()?);
                }
                access
            },
            _rangle: input.parse()?,
            path: input.parse()?,
        })
    }
}
impl ToTokens for CustomAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self._amp.to_tokens(tokens);
        self._langle.to_tokens(tokens);
        self.access.to_tokens(tokens);
        self._rangle.to_tokens(tokens);
        self.path.to_tokens(tokens);
    }
}
impl ForeignPath for CustomAttr {
    fn foreign_path(&self) -> &Path { &self.path }
}

pub type Access<'a> = Option<(bool,Option<&'a Lifetime>)>;

pub fn aggregate_accs<'a:'b,'b>(
    accs: impl IntoIterator<Item = &'a FieldAccess>,
    agg: &mut BTreeMap<&Ident,(&mut Type,Access<'b>)>,
) {
    for acc in accs {
        match acc {
            FieldAccess::Field { mut_tok, ident, lt } => {
                let Some((_,prev_acc)) = agg.get_mut(ident) else { todo!() };
                merge_accs(prev_acc, mut_tok.is_some(), lt);
            },
            FieldAccess::Star { lt, mut_tok, .. } => {
                for (_,prev_acc) in agg.values_mut() {
                    merge_accs(prev_acc, mut_tok.is_some(), lt);
                }
            },
            FieldAccess::Excl { ident, .. } => {
                agg.get_mut(ident).unwrap().1 = None;
            },
        }
    }
}

pub fn merge_accs<'a,'b:'a>(acc1: &mut Access<'a>, mu2: bool, lt2: &'b Option<Lifetime>) {
    if let Some((mu1,lt1)) = acc1 {
        *mu1 = *mu1 || mu2;
        *lt1 = lt2.as_ref().or(*lt1);
    } else {
        *acc1 = Some((mu2,lt2.as_ref()));
    }
}

#[test]
fn test_simple_field_access() {
    let input: TokenStream = quote! {
        <field1, field2>: some::path::to::Type
    };

    let parsed: CustomAttr = parse2(input).expect("Failed to parse");

    assert_eq!(parsed.access.len(), 2);
    let first_access = &parsed.access[0];
    if let FieldAccess::Field { lt, mut_tok, ident } = first_access {
        assert!(lt.is_none());
        assert!(mut_tok.is_none());
        assert_eq!(ident, "field1");
    } else {
        panic!("Expected FieldAccess::Field");
    }

    let path = &parsed.path;
    let path_str = quote!(#path).to_string();
    assert_eq!(path_str, "some :: path :: to :: Type");
}
