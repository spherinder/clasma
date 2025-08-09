use std::collections::BTreeSet;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, punctuated::Punctuated, visit_mut::{self, VisitMut}, FnArg, Ident, ImplItem, Item, Macro, Pat, Signature, Type};

fn extract_args(fields: &BTreeSet<Ident>, sig: &Signature) -> impl Iterator<Item = Result<(Option<bool>, Ident), syn::Error>> {
    sig.inputs.iter().map(|arg| {
        let pat_type = match arg {
            FnArg::Typed(x) => x,
            FnArg::Receiver(r) => return Ok((None, Ident::new("self", r.self_token.span))),
        };
        let Pat::Ident(pat_ident) = &*pat_type.pat else {
            return Err(syn::Error::new_spanned(
                pat_type.clone(),
                "#[clasma] arguments must be normal identifiers",
            ))
        };
        if !fields.contains(&pat_ident.ident) {
            return Ok((None, pat_ident.ident.clone()))
        };
        let Type::Reference(refty) = &*pat_type.ty else {
            return Err(syn::Error::new_spanned(
                pat_type.clone(),
                "#[clasma] arguments must be reference types",
            ))
        };
        return Ok((Some(refty.mutability.is_some()), pat_ident.ident.clone()))
    })
}

fn handle_fn<'a>(fields: &BTreeSet<Ident>, sig: &'a Signature)
    -> Result<(&'a Ident,Vec<TokenStream>,Vec<TokenStream>,Ident,Vec<TokenStream>,Vec<TokenStream>), syn::Error>
{
    let args = extract_args(fields, sig).collect::<Result<Vec<_>,_>>()?;
    let func_name = &sig.ident;
    let match_args: Vec<_> = args.iter()
        .filter(|(mu,_)| mu.is_none())
        .map(|(_,arg)| {
            let id = format_ident!("__{arg}");
            quote! { $#id: expr }
        }).collect();

    let expan_args: Vec<_> = args.iter().map(|(mu,id)| {
        let &Some(mu) = mu else {
            let matchid = format_ident!("__{id}");
            return quote! { $#matchid }
        };
        return if mu {
            quote! { &mut ($st).#id }
        } else {
            quote! { &($st).#id }
        }
    }).collect();


    let mac_scope_name = format_ident!("{func_name}_scope");

    let match_fields: Vec<_> = fields.iter().map(|field| {
        let id = format_ident!("__{field}");
        quote! { $#id: ident }
    }).collect();
    let expan_args_scope: Vec<_> = args.iter().map(|(_,id)| {
        let id = format_ident!("__{id}");
        quote! { $#id }
    }).collect();

    return Ok((func_name, match_args, expan_args, mac_scope_name, match_fields, expan_args_scope));
}

struct ScopeMacroVisitor<'a>(&'a BTreeSet<Ident>);

impl<'a> visit_mut::VisitMut for ScopeMacroVisitor<'a> {
    fn visit_macro_mut(&mut self, mac: &mut Macro) {
        'blk: {
            let Some(last_segment) = mac.path.segments.last() else { break 'blk };
            if !last_segment.ident.to_string().ends_with("_scope") { break 'blk };
            let original_tokens = &mac.tokens;
            let fields = &self.0;
            mac.tokens = quote! { [ #(#fields)* ] #original_tokens };
        }
        visit_mut::visit_macro_mut(self, mac);
    }
}

#[proc_macro_attribute]
pub fn clasma(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let fields: BTreeSet<_> = parse_macro_input!(attr with Punctuated::<Ident, syn::Token![,]>::parse_terminated).into_iter().collect();
    let mut item = parse_macro_input!(item as Item);
    ScopeMacroVisitor(&fields).visit_item_mut(&mut item);

    match item {
        Item::Fn(item_fn) => {
            let (func_name, match_args, expan_args, mac_scope_name, match_fields, expan_args_scope)
                    = match handle_fn(&fields, &item_fn.sig) {
                Ok(x) => x,
                Err(x) => return x.to_compile_error().into(),
            };

            let res = quote! {
                #item_fn

                #[macro_export]
                macro_rules! #func_name {
                    ( < $($lt:lifetime),+ $(, $t:ty)* >, $st:expr #(, #match_args)* ) => {
                        #func_name::< $($lt),* $(, $t)* >( #(#expan_args),* );
                    };
                    ( < $($t:ty),+ >, $st:expr #(, #match_args)* ) => {
                        #func_name::< $($t),* >( #(#expan_args),* );
                    };

                    ( $st:expr #(, #match_args)* ) => {
                        #func_name( #(#expan_args),* );
                    };
                }

                #[macro_export]
                macro_rules! #mac_scope_name {
                    ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* > #(, #match_args)* ) => {
                        #func_name::< $($lt),* $(, $t)* >( #(#expan_args_scope),* );
                    };
                    ( [ #(#match_fields)* ] < $($t:ty),+ > #(, #match_args)* ) => {
                        #func_name::< $($t),* >( #(#expan_args_scope),* );
                    };

                    ( [ #(#match_fields)* ] #(#match_args),* ) => {
                        #func_name( #(#expan_args_scope),* );
                    };
                }
            };
            return res.into();
        },
        Item::Impl(item_impl) => {
            if item_impl.trait_.is_some() {
                return syn::Error::new_spanned(
                    item_impl,
                    "clasma::partial currently does not support `impl Trait` blocks.",
                ).to_compile_error().into();
            }
            let Some(st_name) = ('blk: {
                    let Type::Path(st_path) = &*item_impl.self_ty else { break 'blk None };
                    let Some(st_name) = st_path.path.segments.last() else { break 'blk None };
                    Some(&st_name.ident)
            }) else {
                return syn::Error::new_spanned(
                    item_impl,
                    "clasma::partial only supports `impl` blocks of `path::to::Type`",
                ).to_compile_error().into();
            };



            let macs: Result<Vec<_>, syn::Error> = item_impl.items.iter().filter_map(|item| {
                let ImplItem::Fn(f) = item else { return None };

                if !f.sig.inputs.iter().any(|arg| {
                    let FnArg::Typed(pat_type) = arg else { return false };
                    let Pat::Ident(pat_ident) = &*pat_type.pat else { return false };
                    fields.contains(&pat_ident.ident)
                }) {
                    return None
                }
                // TODO produce a warning, if it "seems" like user is attempting to incorrectly use macro, instead of ignoring the `Err`
                return handle_fn(&fields, &f.sig).ok();
            }).map(|(func_name, match_args, expan_args, mac_scope_name, match_fields, expan_args_scope)| {
                // It's really annoying that lifetimes and types can not be parsed unambiguously in one rule.
                // TODO use a tt-muncher to make less verbose
                return Ok(quote! {
                    #[macro_export]
                    macro_rules! #func_name {
                        ( < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($lt2:lifetime),+ $(, $t2:ty)* >, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($lt1),* $(, $t1)* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args),* );
                        };
                        ( < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($t2:ty),+ >, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($lt1),* $(, $t1)* >::#func_name::< $($t2),* >( #(#expan_args),* );
                        };
                        ( < $($t1:ty),+ >::< $($lt2:lifetime),+ $(, $t2:ty)* >, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($t1),* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args),* );
                        };
                        ( < $($t1:ty),+ >::< $($t2:ty),+ >, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($t1),* >::#func_name::< $($t2),* >( #(#expan_args),* );
                        };

                        ( < $($lt:lifetime),+ $(, $t:ty)* >::, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($lt),* $(, $t)* >::#func_name( #(#expan_args),* );
                        };
                        ( < $($t:ty),+ >::, $st:expr #(, #match_args)* ) => {
                            #st_name::< $($t),* >::#func_name( #(#expan_args),* );
                        };

                        ( < $($lt:lifetime),+ $(, $t:ty)* >, $st:expr #(, #match_args)* ) => {
                            #st_name::#func_name::< $($lt),* $(, $t)* >( #(#expan_args),* );
                        };
                        ( < $($t:ty)+ >, $st:expr #(, #match_args)* ) => {
                            #st_name::#func_name::< $($t),* >( #(#expan_args),* );
                        };

                        ( $st:expr #(, #match_args)* ) => {
                            #st_name::#func_name( #(#expan_args),* );
                        };
                    }

                    #[macro_export]
                    macro_rules! #mac_scope_name {
                        ( [ #(#match_fields)* ] < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($lt2:lifetime),+ $(, $t2:ty)* > #(, #match_args)* ) => {
                            #st_name::< $($lt1),* $(, $t1)* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args_scope),* );
                        };
                        ( [ #(#match_fields)* ] < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($t2:ty),+ > #(, #match_args)* ) => {
                            #st_name::< $($lt1),* $(, $t1)* >::#func_name::< $($t2),* >( #(#expan_args_scope),* );
                        };
                        ( [ #(#match_fields)* ] < $($t1:ty),+ >::< $($lt2:lifetime),+ $(, $t2:ty)* > #(, #match_args)* ) => {
                            #st_name::< $($t1),* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args_scope),* );
                        };
                        ( [ #(#match_fields)* ] < $($t1:ty),+ >::< $($t2:ty),+ > #(, #match_args)* ) => {
                            #st_name::< $($t1),* >::#func_name::< $($t2),* >( #(#expan_args_scope),* );
                        };

                        ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* >:: #(, #match_args)* ) => {
                            #st_name::< $($lt),* $(, $t)* >::#func_name( #(#expan_args_scope),* );
                        };
                        ( [ #(#match_fields)* ] < $($t:ty),+ >:: #(, #match_args)* ) => {
                            #st_name::< $($t),* >::#func_name( #(#expan_args_scope),* );
                        };

                        ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* > #(, #match_args)* ) => {
                            #st_name::#func_name::< $($lt),* $(, $t)* >( #(#expan_args_scope),* );
                        };
                        ( [ #(#match_fields)* ] < $($t:ty)+ > #(, #match_args)* ) => {
                            #st_name::#func_name::< $($t),* >( #(#expan_args_scope),* );
                        };

                        ( [ #(#match_fields)* ] #(#match_args),* ) => {
                            #st_name::#func_name( #(#expan_args_scope),* );
                        };
                    }
                });
            }).collect();

            let macs = match macs {Ok(x) => x, Err(x) => return x.to_compile_error().into()};
            let res = quote! {
                #item_impl

                #(#macs)*
            };
            return res.into();
        },
        _ => {
            return syn::Error::new_spanned(
                item,
                "clasma::partial must be applied to an `fn` or `impl` block.",
            ).to_compile_error().into()
        },
    }
}
