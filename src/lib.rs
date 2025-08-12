use std::collections::BTreeSet;
use macro_magic::{import_tokens_attr, mm_core::export_tokens_internal};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, visit_mut::{self, VisitMut}, FnArg, Ident, ImplItem, Item, ItemStruct, Macro, Pat, PathArguments, Signature, Type};

fn extract_args<'a,'b:'a>(fields: &BTreeSet<&Ident>, sig: &'a Signature, selfident: &'b Ident) -> impl Iterator<Item = Result<(Option<bool>, &'a Ident), syn::Error>> {
    sig.inputs.iter().map(move |arg| {
        let FnArg::Typed(pat_type) = arg else { return Ok((None, selfident)) };
        let Pat::Ident(pat_ident) = &*pat_type.pat else {
            return Err(syn::Error::new_spanned(
                pat_type.clone(),
                "#[clasma] arguments must be normal identifiers",
            ))
        };
        if !fields.contains(&pat_ident.ident) {
            return Ok((None, &pat_ident.ident))
        };
        let Type::Reference(refty) = &*pat_type.ty else {
            return Err(syn::Error::new_spanned(
                pat_type.clone(),
                "#[clasma] arguments must be reference types",
            ))
        };
        return Ok((Some(refty.mutability.is_some()), &pat_ident.ident))
    })
}

fn mk_fn_quotes<'a>(fields: &BTreeSet<&Ident>, args: &Vec<(Option<bool>, &'a Ident)>)
    -> (Vec<TokenStream>,Vec<TokenStream>,Vec<TokenStream>,Vec<TokenStream>,Vec<&'a Ident>)
{
    let (mac_arg_names, match_args): (Vec<_>,Vec<_>) = args.iter()
        .filter(|(mu,_)| mu.is_none())
        .map(|&(_,arg)| {
            let id = format_ident!("_{arg}");
            (arg, quote! { $#id: expr })
        }).unzip();

    let expan_args: Vec<_> = args.iter().map(|&(mu,id)| {
        let Some(mu) = mu else {
            let matchid = format_ident!("_{id}");
            return quote! { $#matchid }
        };
        return if mu {
            quote! { &mut ($st).#id }
        } else {
            quote! { &($st).#id }
        }
    }).collect();


    let match_fields: Vec<_> = fields.iter().map(|field| {
        let id = format_ident!("_{field}");
        quote! { $#id: ident }
    }).collect();
    let expan_args_scope: Vec<_> = args.iter().map(|(_,id)| {
        let id = format_ident!("_{id}");
        quote! { $#id }
    }).collect();

    return (match_args, expan_args, match_fields, expan_args_scope, mac_arg_names);
}

struct ScopeMacroVisitor<'a,'b>(&'a BTreeSet<&'b Ident>);

impl<'a,'b> VisitMut for ScopeMacroVisitor<'a,'b> {
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
    let item = parse_macro_input!(item as Item);
    let Item::Struct(_) = &item else {
        return syn::Error::new_spanned(
            item,
            "#[clasma] must attribute a struct",
        ).to_compile_error().into();
    };
    match export_tokens_internal(attr, item.to_token_stream(), true, true) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[import_tokens_attr]
#[proc_macro_attribute]
pub fn partial(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut st_item = parse_macro_input!(attr as ItemStruct);
    let selfident = &format_ident!("self");

    let (item, fields) = {
        let mut item = parse_macro_input!(item as Item);
        let Some(fields): Option<BTreeSet<_>> = st_item.fields.iter_mut().map(
            // HACK: changing span allows `ScopeMacroVisitor` to pass fields to `foo_scope!`
            |x| x.ident.as_mut().map(|x| { x.set_span(item.span()); &*x})
        ).collect() else {
            return syn::Error::new_spanned(
                st_item,
                "clasma can only partially borrow named structs",
            ).to_compile_error().into();
        };
        ScopeMacroVisitor(&fields).visit_item_mut(&mut item);
        (item,fields)
    };

    match item {
        Item::Fn(item_fn) => {
            let args = match extract_args(&fields, &item_fn.sig, selfident).collect::<Result<Vec<_>,_>>() {
                Ok(x) => x, Err(x) => return x.to_compile_error().into(),
            };
            let (match_args, expan_args, match_fields, expan_args_scope, arg_names) = mk_fn_quotes(&fields, &args);
            let func_name = &item_fn.sig.ident;
            let scope_name = format_ident!("{func_name}_scope");

            let res = quote! {
                #item_fn

                #[macro_export]
                macro_rules! #func_name {
                    ( < $($lt:lifetime),+ $(, $t:ty)* >, $st:expr #(, #match_args)* ) => {
                        self::#func_name::< $($lt),* $(, $t)* >( #(#expan_args),* )
                    };
                    ( < $($t:ty),+ >, $st:expr #(, #match_args)* ) => {
                        self::#func_name::< $($t),* >( #(#expan_args),* )
                    };

                    ( $st:expr #(, #match_args)* ) => {
                        self::#func_name( #(#expan_args),* )
                    };

                    ($($other:tt)*) => {
                        compile_error!(concat!(
                            "Invalid syntax for `",stringify!(#func_name),"!`. ",
                            "Usage: `",stringify!(#func_name),"!(<optional_generics>, struct_instance", stringify!(#(, #arg_names )*), ")`"
                        ));
                    };
                }

                #[macro_export]
                macro_rules! #scope_name {
                    ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* > #(, #match_args)* ) => {
                        self::#func_name::< $($lt),* $(, $t)* >( #(#expan_args_scope),* )
                    };
                    ( [ #(#match_fields)* ] < $($t:ty),+ > #(, #match_args)* ) => {
                        self::#func_name::< $($t),* >( #(#expan_args_scope),* )
                    };

                    ( [ #(#match_fields)* ] #(#match_args),* ) => {
                        self::#func_name( #(#expan_args_scope),* )
                    };
                    ($($other:tt)*) => {
                        compile_error!(concat!(
                            "Invalid syntax for `",stringify!(#scope_name),"!`. ",
                            "Usage: `",stringify!(#scope_name),"!(<optional_generics>", stringify!(#( #arg_names),*), ")`"
                        ));
                    };
                    ( $($other:tt)* ) => {
                        compile_error!(concat!(
                            "The `",
                            stringify!(#scope_name),
                            "` macro must be called from within a function or `impl` block annotated with `#[clasma::partial]`."
                        ))
                    };
                }
            };
            return res.into();
        },
        Item::Impl(item_impl) => {
            if item_impl.trait_.is_some() {
                return syn::Error::new_spanned(
                    item_impl,
                    "clasma currently does not support `impl Trait` blocks.",
                ).to_compile_error().into();
            }
            let Some(st_path) = ('blk: {
                let Type::Path(st_path) = &*item_impl.self_ty else { break 'blk None };
                let mut st_path = st_path.clone();
                let Some(st_name) = st_path.path.segments.last_mut() else { break 'blk None };
                st_name.arguments = PathArguments::None;
                Some(st_path)
            }) else {
                return syn::Error::new_spanned(
                    item_impl,
                    "clasma only supports `impl` blocks of `path::to::Type`",
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

                let Ok(args) = extract_args(&fields, &f.sig, selfident).collect::<Result<Vec<_>,_>>() else {
                    // TODO produce a warning, if it "seems" like user is attempting to incorrectly use macro, instead of ignoring the `Err`
                    return None
                };
                return Some((&f.sig.ident, mk_fn_quotes(&fields, &args)));
            }).map(|(func_name, (match_args, expan_args, match_fields, expan_args_scope, arg_names))| {
                let scope_name = format_ident!("{func_name}_scope");

                // It's really annoying that lifetimes and types can not be parsed unambiguously in one rule.
                // TODO use a tt-muncher to make less verbose
                return Ok(quote! {
                    #[macro_export]
                    macro_rules! #func_name {
                        ( < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($lt2:lifetime),+ $(, $t2:ty)* >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($lt1),* $(, $t1)* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args),* )
                        };
                        ( < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($t2:ty),+ >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($lt1),* $(, $t1)* >::#func_name::< $($t2),* >( #(#expan_args),* )
                        };
                        ( < $($t1:ty),+ >::< $($lt2:lifetime),+ $(, $t2:ty)* >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($t1),* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args),* )
                        };
                        ( < $($t1:ty),+ >::< $($t2:ty),+ >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($t1),* >::#func_name::< $($t2),* >( #(#expan_args),* )
                        };

                        ( < $($lt:lifetime),+ $(, $t:ty)* >::, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($lt),* $(, $t)* >::#func_name( #(#expan_args),* )
                        };
                        ( < $($t:ty),+ >::, $st:expr #(, #match_args)* ) => {
                            self::#st_path::< $($t),* >::#func_name( #(#expan_args),* )
                        };

                        ( < $($lt:lifetime),+ $(, $t:ty)* >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::#func_name::< $($lt),* $(, $t)* >( #(#expan_args),* )
                        };
                        ( < $($t:ty)+ >, $st:expr #(, #match_args)* ) => {
                            self::#st_path::#func_name::< $($t),* >( #(#expan_args),* )
                        };

                        ( $st:expr #(, #match_args)* ) => {
                            self::#st_path::#func_name( #(#expan_args),* )
                        };

                        ($($other:tt)*) => {
                            compile_error!(concat!(
                                "Invalid syntax for `",stringify!(#func_name),"!`. ",
                                "Usage: `",stringify!(#func_name),"!(<optional_impl_generics>::<optional_fn_generics>, struct_instance", stringify!(#(, #arg_names )*), ")`"
                            ));
                        };
                    }

                    #[macro_export]
                    macro_rules! #scope_name {
                        ( [ #(#match_fields)* ] < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($lt2:lifetime),+ $(, $t2:ty)* > #(, #match_args)* ) => {
                            self::#st_path::< $($lt1),* $(, $t1)* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args_scope),* )
                        };
                        ( [ #(#match_fields)* ] < $($lt1:lifetime),+ $(, $t1:ty)* >::< $($t2:ty),+ > #(, #match_args)* ) => {
                            self::#st_path::< $($lt1),* $(, $t1)* >::#func_name::< $($t2),* >( #(#expan_args_scope),* )
                        };
                        ( [ #(#match_fields)* ] < $($t1:ty),+ >::< $($lt2:lifetime),+ $(, $t2:ty)* > #(, #match_args)* ) => {
                            self::#st_path::< $($t1),* >::#func_name::< $($lt2),* $(, $t2)* >( #(#expan_args_scope),* )
                        };
                        ( [ #(#match_fields)* ] < $($t1:ty),+ >::< $($t2:ty),+ > #(, #match_args)* ) => {
                            self::#st_path::< $($t1),* >::#func_name::< $($t2),* >( #(#expan_args_scope),* )
                        };

                        ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* >:: #(, #match_args)* ) => {
                            self::#st_path::< $($lt),* $(, $t)* >::#func_name( #(#expan_args_scope),* )
                        };
                        ( [ #(#match_fields)* ] < $($t:ty),+ >:: #(, #match_args)* ) => {
                            self::#st_path::< $($t),* >::#func_name( #(#expan_args_scope),* )
                        };

                        ( [ #(#match_fields)* ] < $($lt:lifetime),+ $(, $t:ty)* > #(, #match_args)* ) => {
                            self::#st_path::#func_name::< $($lt),* $(, $t)* >( #(#expan_args_scope),* )
                        };
                        ( [ #(#match_fields)* ] < $($t:ty)+ > #(, #match_args)* ) => {
                            self::#st_path::#func_name::< $($t),* >( #(#expan_args_scope),* )
                        };

                        ( [ #(#match_fields)* ] #(#match_args),* ) => {
                            self::#st_path::#func_name( #(#expan_args_scope),* )
                        };

                        ( [ #(#match_fields)* ] $($other:tt)* ) => {
                            compile_error!(concat!(
                                "Invalid syntax for `",stringify!(#scope_name),"!`. ",
                                "Usage: `",stringify!(#scope_name),"!(<optional_impl_generics>::<optional_fn_generics>", stringify!(#( #arg_names),*), ")`"
                            ));
                        };

                        ($($other:tt)*) => {
                            compile_error!(concat!(
                                "The `",
                                stringify!(#scope_name),
                                "` macro must be called from within a function or `impl` block annotated with `#[clasma::partial]`."
                            ))
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
                "clasma must be applied to an outer `fn` definition, an `impl` block, or a `struct` definition.",
            ).to_compile_error().into()
        },
    }
}
