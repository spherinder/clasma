use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, Attribute, FnArg, ImplItem, Item, Pat, Signature, Type};

fn is_clasma_field_attr(attr: &Attribute) -> bool {
    let Some(first_seg) = attr.path().segments.first() else { return false };
    first_seg.ident == "clasma"
}

fn compute_match_args(sig: &mut Signature) -> Result<(Vec<TokenStream>, Vec<TokenStream>), syn::Error> {
    let mut match_args = Vec::new();
    let mut expan_args = Vec::new();
    for (i,arg) in sig.inputs.iter().enumerate() {
        if 'blk: {
            let FnArg::Typed(pat_type) = arg else { break 'blk true };
            if !pat_type.attrs.iter().any(is_clasma_field_attr) { break 'blk true };
            let Type::Reference(refty) = &*pat_type.ty else {
                return Err(syn::Error::new_spanned(
                    pat_type,
                    "#[clasma] arguments must be reference types",
                ))
            };
            let Pat::Ident(pat_ident) = &*pat_type.pat else {
                return Err(syn::Error::new_spanned(
                    pat_type,
                    "#[clasma] arguments must be normal identifiers",
                ))
            };

            let field = &pat_ident.ident;
            expan_args.push(match refty.mutability {
                Some(_) => quote! { &mut ($st).#field },
                None => quote! { &($st).#field },
            });
            false
        } {
            let matcharg = format_ident!("__arg{i}");
            match_args.push(quote! { $#matcharg:expr });
            expan_args.push(quote! { $#matcharg });
        }
    }

    for arg in sig.inputs.iter_mut() {
        let FnArg::Typed(pat_type) = arg else { continue };
        pat_type.attrs.retain(|a| !is_clasma_field_attr(a));
    }
    return Ok((match_args, expan_args));
}

#[proc_macro_attribute]
pub fn partial(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as Item);
    match item {
        Item::Fn(mut item_fn) => {
            let func_name = &item_fn.sig.ident.to_token_stream();
            let (match_args, expan_args) = match compute_match_args(&mut item_fn.sig) {
                Ok(x) => x,
                Err(x) => return x.to_compile_error().into(),
            };
            return quote! {
                #item_fn

                #[macro_export]
                macro_rules! #func_name {
                    ( < $($lt:lifetime),+ $(, $t:ty)* >, $st:expr, #(#match_args),* ) => {
                        #func_name::< $($lt),* $(, $t)* >( #(#expan_args),* );
                    };
                    ( < $($t:ty),+ >, $st:expr, #(#match_args),* ) => {
                        #func_name::< $($t),* >( #(#expan_args),* );
                    };

                    ( $st:expr, #(#match_args),* ) => {
                        #func_name( #(#expan_args),* );
                    };
                }
            }.into();
        },
        Item::Impl(mut item_impl) => {
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

            let macs: Result<Vec<_>, syn::Error> = item_impl.items.iter_mut().filter_map(|item| {
                if let ImplItem::Fn(f) = item {Some(f)} else {None}
            }).map(|f| {
                let (match_args, expan_args) = compute_match_args(&mut f.sig)?;
                let func_name = &f.sig.ident;
                // It's really annoying that lifetimes and types can not be parsed unambiguously in one rule
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
