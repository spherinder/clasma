mod field_access;

use std::collections::{BTreeMap, HashMap};
use field_access::{aggregate_accs, Access, CustomAttr};
use macro_magic::{import_tokens_attr, mm_core::{export_tokens_internal}};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, token, visit_mut::{self, VisitMut}, Expr, ExprPath, FnArg, GenericArgument, GenericParam, Ident, ImplItem, Item, ItemFn, ItemImpl, ItemStruct, Lifetime, Macro, Pat, PatIdent, PatType, Path, PathArguments, PathSegment, Signature, Token, Type, TypePath, TypeReference};

fn extract_args<'a,'b:'a,T>(fields: &BTreeMap<&Ident,T>, sig: &'a Signature, selfident: Option<&'b Ident>) -> impl Iterator<Item = Result<(Option<bool>, &'a Ident), syn::Error>> {
    sig.inputs.iter().map(move |arg| {
        let FnArg::Typed(pat_type) = arg else { return Ok((None, selfident.unwrap())) };
        let Pat::Ident(pat_ident) = &*pat_type.pat else {
            return Err(syn::Error::new_spanned(
                pat_type.clone(),
                "#[clasma] arguments must be normal identifiers",
            ))
        };
        if !fields.contains_key(&pat_ident.ident) {
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

fn mk_fn_quotes<'a,T>(fields: &BTreeMap<&Ident,T>, args: &Vec<(Option<bool>, &'a Ident)>)
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


    let match_fields: Vec<_> = fields.keys().map(|field| {
        let id = format_ident!("_{field}");
        quote! { $#id: ident }
    }).collect();
    let expan_args_scope: Vec<_> = args.iter().map(|(_,id)| {
        let id = format_ident!("_{id}");
        quote! { $#id }
    }).collect();

    return (match_args, expan_args, match_fields, expan_args_scope, mac_arg_names);
}

struct ScopeMacroVisitor<'a,'b,T>(&'a BTreeMap<&'b Ident, T>);

impl<'a,'b,T> VisitMut for ScopeMacroVisitor<'a,'b,T> {
    fn visit_macro_mut(&mut self, mac: &mut Macro) {
        'blk: {
            let Some(last_segment) = mac.path.segments.last() else { break 'blk };
            if !last_segment.ident.to_string().ends_with("_scope") { break 'blk };
            let original_tokens = &mac.tokens;
            let fields = self.0.keys();
            mac.tokens = quote! { [ #(#fields)* ] #original_tokens };
        }
        visit_mut::visit_macro_mut(self, mac);
    }
}

struct GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2> {
    ls: HashMap<&'l1 Ident, &'l2 Lifetime>,
    ts: HashMap<&'t1 Ident, &'t2 Type>,
    cs: HashMap<&'c1 Ident, &'c2 Expr>,
}
// struct GenericSubstitutor<'l1,'l2,'l3>(&'l1 HashMap<&'l2 Ident, &'l3 GenericArgument);

impl<'ls2,'ls3,'ts2,'ts3,'c2,'c3> VisitMut for GenericSubstitutor<'ls2,'ls3,'ts2,'ts3,'c2,'c3> {
// impl<> VisitMut for GenericSubstitutor<'l1,'l2,'l3> {
    fn visit_type_mut(&mut self, t: &mut Type) {
        'blk: {
            let Type::Path(TypePath {qself: None, path, ..}) = &*t else { break 'blk };
            let Some(PathSegment {arguments: PathArguments::None, ident}) = path.segments.first() else { break 'blk };
            let Some(&ty) = self.ts.get(ident) else { break 'blk };
            *t = ty.clone();
            return;
        }
        visit_mut::visit_type_mut(self, t);
    }

    fn visit_lifetime_mut(&mut self, l: &mut Lifetime) {
        let Some(&lt) = self.ls.get(&l.ident) else { return };
        *l = lt.clone();
    }

    fn visit_generic_argument_mut(&mut self, arg: &mut GenericArgument) {
        'blk: {
            let GenericArgument::Const(expr) = arg else { break 'blk };
            let Expr::Path(ExprPath {qself: None, path, ..}) = &*expr else { unreachable!() };
            if path.segments.len() != 1 { unreachable!() };
            let Some(&c) = self.cs.get(&path.segments[0].ident) else { break 'blk };
            *expr = c.clone();
            return;
        }
        visit_mut::visit_generic_argument_mut(self, arg);
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

fn mk_subst_maps<'a:'l1+'t1+'c1, 'b:'l2+'t2+'c2, 'l1,'l2,'t1,'t2,'c1,'c2>(
    gen_args: impl IntoIterator<Item = &'a GenericParam>,
    args: impl IntoIterator<Item = &'b GenericArgument>,
    subst: &mut GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2>,
) {
    let mut subst_pairs = gen_args.into_iter().zip(args.into_iter()).peekable();
    while let Some((GenericParam::Lifetime(gen_lt), GenericArgument::Lifetime(lt))) = subst_pairs.peek() {
        subst.ls.insert(&gen_lt.lifetime.ident, lt);
        subst_pairs.next();
    }
    while let Some(arg_pair) = &subst_pairs.next() {
        match arg_pair {
            (GenericParam::Type(gen_t), GenericArgument::Type(t)) => {
                subst.ts.insert(&gen_t.ident, t);
            },
            (GenericParam::Const(gen_c), GenericArgument::Const(c)) => {
                subst.cs.insert(&gen_c.ident, c);
            },
            _ => panic!("something wrong with ({}, {})", arg_pair.0.to_token_stream(), arg_pair.1.to_token_stream()),

        }
    }
}

#[import_tokens_attr]
#[with_custom_parsing(CustomAttr)]
#[proc_macro_attribute]
pub fn partial(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let CustomAttr { access, path, .. } = parse_macro_input!(__custom_tokens as CustomAttr);
    let mut st_item = parse_macro_input!(attr as ItemStruct);

    let (item, fields) = {
        let mut item = parse_macro_input!(item as ItemFn);
        let Some(mut fields): Option<BTreeMap<_,(_,Access)>> = st_item.fields.iter_mut().map(|field| {
            // HACK: changing span allows `ScopeMacroVisitor` to pass fields to `foo_scope!`
            field.ident.as_mut().map(|id| { id.set_span(item.span()); (&*id, (&mut field.ty, None))})
        }).collect() else {
            return syn::Error::new_spanned(
                st_item,
                "clasma can only partially borrow named structs",
            ).to_compile_error().into();
        };
        ScopeMacroVisitor(&fields).visit_item_fn_mut(&mut item);

        if let PathArguments::AngleBracketed(st_concrete) = &path.segments.last().unwrap().arguments {
            let mut subst = GenericSubstitutor { ls: HashMap::new(), ts: HashMap::new(), cs: HashMap::new() };
            mk_subst_maps(st_item.generics.params.iter(), st_concrete.args.iter(), &mut subst);
            for (t,_) in fields.values_mut() {
                subst.visit_type_mut(t);
            }
        }

        aggregate_accs(access.iter(), &mut fields);

        item.sig.inputs.extend(fields.iter().filter_map(|(&field, &(ref ty,ref_info))| {
            let Some((mu,lt)) = ref_info else { return None };
            Some(FnArg::Typed(PatType {
                pat: Box::new(Pat::Ident(PatIdent {
                    ident: field.clone(),
                    attrs:Vec::new(),by_ref:None,mutability:None,subpat:None,
                })),
                ty: Box::new(Type::Reference(TypeReference {
                    lifetime: lt.cloned(),
                    mutability: if mu { Some(token::Mut(Span::call_site())) } else { None },
                    elem: Box::new((*ty).clone()),
                    and_token: token::And(Span::call_site()),
                })),
                attrs: Vec::new(),
                colon_token: token::Colon(Span::call_site()),
            }))
        }));

        (item, fields)
    };
    let args = match extract_args(&fields, &item.sig, None).collect::<Result<Vec<_>,_>>() {
        Ok(x) => x, Err(x) => return x.to_compile_error().into(),
    };
    let (match_args, expan_args, match_fields, expan_args_scope, arg_names) = mk_fn_quotes(&fields, &args);
    let func_name = &item.sig.ident;
    let scope_name = format_ident!("{func_name}_scope");

    let res = quote! {
        #item

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
            ( $($other:tt)* ) => {
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
}

#[import_tokens_attr]
#[proc_macro_attribute]
pub fn partial_impl(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut item = parse_macro_input!(item as ItemImpl);
    let selfident = &format_ident!("self");
    if item.trait_.is_some() {
        return syn::Error::new_spanned(
            item,
            "clasma currently does not support `impl Trait` blocks.",
        ).to_compile_error().into();
    }
    let Some(st_path) = ('blk: {
        let Type::Path(st_path) = &*item.self_ty else { break 'blk None };
        let mut st_path = st_path.clone();
        let Some(st_name) = st_path.path.segments.last_mut() else { break 'blk None };
        st_name.arguments = PathArguments::None;
        Some(st_path)
    }) else {
        return syn::Error::new_spanned(
            item,
            "clasma only supports `impl` blocks of `path::to::Type`",
        ).to_compile_error().into();
    };

    let macs: Result<Vec<_>, syn::Error> = item.items.iter().filter_map(|item| {
        let ImplItem::Fn(f) = item else { return None };

        if !f.sig.inputs.iter().any(|arg| {
            let FnArg::Typed(pat_type) = arg else { return false };
            let Pat::Ident(pat_ident) = &*pat_type.pat else { return false };
            fields.contains_key(&pat_ident.ident)
        }) {
            return None
        }

        let Ok(args) = extract_args(&fields, &f.sig, Some(selfident)).collect::<Result<Vec<_>,_>>() else {
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
        #item

        #(#macs)*
    };
    return res.into();
    // _ => {
    //     return syn::Error::new_spanned(
    //         item,
    //         "clasma must be applied to an outer `fn` definition, an `impl` block, or a `struct` definition.",
    //     ).to_compile_error().into()
    // }
}
