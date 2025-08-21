#![allow(clippy::needless_return)]
mod borrow_attr;

use std::{borrow::Borrow, collections::{BTreeMap, HashMap, HashSet}, mem};
use borrow_attr::{BorrowAttr, FieldAccess};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{braced, bracketed, parse::{ParseStream, Parser}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token, visit_mut::{self, VisitMut}, Attribute, Expr, ExprCall, ExprField, ExprPath, ExprReference, FnArg, GenericArgument, GenericParam, Ident, ImplItem, Item, ItemFn, ItemImpl, ItemStruct, Lifetime, LitInt, Member, Meta, MetaList, Pat, PatIdent, PatType, Path, PathArguments, PathSegment, Signature, Type, TypePath, TypeReference};

type Access<'a> = Option<(bool,Option<&'a Lifetime>)>;

fn merge_accs<'a,'b:'a>(acc: &mut Access<'a>, mu2: bool, lt2: &'b Option<Lifetime>) {
    if let Some((mu1,lt1)) = acc {
        *mu1 = mu2;
        *lt1 = lt2.as_ref().or(*lt1);
    } else {
        *acc = Some((mu2,lt2.as_ref()));
    }
}

fn aggregate_accs<'a:'b,'b, T: Ord + Borrow<Ident>, U>(
    accs: impl IntoIterator<Item = &'a FieldAccess>,
    agg: &mut Ctx<'b,T,U>,
) -> syn::Result<()> {
    for acc in accs {
        match acc {
            FieldAccess::Field(lt, mu, ident) => {
                let Some((_,_,prev_acc)) = agg.get_mut(ident) else {
                    return Err(syn::Error::new_spanned(acc, "unknown struct field"))
                };
                merge_accs(prev_acc, *mu, lt);
            },
            FieldAccess::Star(lt, mu) => {
                for (_,_,prev_acc) in agg.values_mut() {
                    merge_accs(prev_acc, *mu, lt);
                }
            },
            FieldAccess::Excl(ident) => {
                agg.get_mut(ident).unwrap().2 = None;
            },
        }
    }
    Ok(())
}

fn extend_fn_sig<T: Ord + Borrow<Ident>, U: Borrow<Type>>(
    sig: &mut Signature, fields: &Ctx<T,U>,
) {
    sig.inputs.extend(fields.iter().filter_map(|(field, &(_, ref ty, borrowed))| {
        let (mu,lt) = borrowed?;
        Some(FnArg::Typed(PatType {
            pat: Box::new(Pat::Ident(PatIdent {
                ident: field.borrow().clone(),
                attrs:Vec::new(),by_ref:None,mutability:None,subpat:None,
            })),
            ty: Box::new(Type::Reference(TypeReference {
                lifetime: lt.cloned(),
                mutability: if mu { Some(token::Mut::default()) } else { None },
                elem: Box::new(ty.borrow().clone()),
                and_token: token::And::default(),
            })),
            attrs:Vec::new(),colon_token:token::Colon::default(),
        }))
    }));
}

struct GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2> {
    ls: HashMap<&'l1 Ident, &'l2 Lifetime>,
    ts: HashMap<&'t1 Ident, &'t2 Type>,
    cs: HashMap<&'c1 Ident, &'c2 Expr>,
}

impl<'l1,'l2,'t1,'t2,'c1,'c2> GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2> {
    fn new() -> Self {
        Self { ls: HashMap::new(), ts: HashMap::new(), cs: HashMap::new() }
    }
}

impl<'l1,'l2,'t1,'t2,'c1,'c2> VisitMut for GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2> {
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

fn mk_subst_maps<'a:'l1+'t1+'c1, 'b:'l2+'t2+'c2, 'l1,'l2,'t1,'t2,'c1,'c2>(
    gen_args: impl IntoIterator<Item = &'a GenericParam>,
    args: impl IntoIterator<Item = &'b GenericArgument>,
    subst: &mut GenericSubstitutor<'l1,'l2,'t1,'t2,'c1,'c2>,
) {
    let mut subst_pairs = gen_args.into_iter().zip(args).peekable();
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
            _ => unreachable!("something wrong with ({}, {})", arg_pair.0.to_token_stream(), arg_pair.1.to_token_stream()),

        }
    }
}

#[proc_macro]
pub fn __scoped(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let fn_call = match (|input: ParseStream| {
        let mut fn_call = {
            let content; braced!(content in input);
            content.parse::<ExprCall>()?
        };
        let mut st_exprs = Vec::new();
        {
            let content; bracketed!(content in input);
            while !content.is_empty() {
                let inner; braced!(inner in content);
                st_exprs.push(if inner.peek(token::DotDot) {
                    Err(inner.parse::<token::DotDot>()?.span())
                } else {
                    Ok(inner.parse::<Expr>()?)
                });
            }
        }
        let content; bracketed!(content in input);
        while !content.is_empty() {
            let mut field = content.parse::<Ident>()?;
            let i = content.parse::<LitInt>()?.base10_parse::<usize>()?;
            let mu = if content.peek(token::Mut) {
                Some(content.parse::<token::Mut>()?)
            } else {
                None
            };
            match &st_exprs.get(i) {
                Some(Err(span)) => {
                    field.set_span(*span);
                    fn_call.args.push(Expr::Path(ExprPath {path: Path { segments: {
                        let mut x = Punctuated::new();
                        x.push_value(PathSegment {
                            ident: field,
                            arguments: PathArguments::None,
                        });
                        x
                    }, leading_colon: None }, attrs:Vec::new(), qself:None }))
                },
                Some(Ok(st_expr)) => {
                    fn_call.args.push(Expr::Reference(ExprReference {
                        mutability: mu,
                        expr: Box::new(Expr::Field(ExprField {
                            base: Box::new(st_expr.clone()),
                            member: Member::Named(field),
                            dot_token:token::Dot::default(),attrs:Vec::new()
                        })), and_token:token::And::default(),attrs:Vec::new()
                    }));
                },
                _ => {
                    return Err(syn::Error::new(
                        fn_call.paren_token.span.close(),
                        "expected a partial borrow argument here",
                    ))
                },
            }
        }
        return Ok(fn_call)
    }).parse(input) {
        Ok(x) => x,
        Err(x) => return x.into_compile_error().into()
    };
    let res = fn_call.into_token_stream();
    return res.into()
}

type Ctx<'t,T,U> = BTreeMap<T, (usize, U, Option<(bool, Option<&'t Lifetime>)>)>;

#[proc_macro]
pub fn __clasma_fn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (mut stdefs, borrows, mut item_fn) = match (|input: ParseStream| {
        let stdefs = {
            let content; braced!(content in input);
            Punctuated::<ItemStruct, token::Semi>::parse_terminated(&content)?
        };
        let borrows = {
            let content; bracketed!(content in input);
            Punctuated::<BorrowAttr, token::Comma>::parse_terminated(&content)?
        };
        let item_fn: ItemFn = input.parse()?;
        return Ok((stdefs, borrows, item_fn))
    }).parse(input) {
        Ok(x) => x,
        Err(x) => return x.into_compile_error().into()
    };

    for (borrow, stdef) in borrows.iter().zip(stdefs.iter_mut()) {
        let PathArguments::AngleBracketed(ref borrow_genargs) = borrow.path.segments.last().unwrap().arguments else { continue };
        let mut subst = GenericSubstitutor::new();
        mk_subst_maps(stdef.generics.params.iter(), borrow_genargs.args.iter(), &mut subst);
        subst.visit_fields_mut(&mut stdef.fields);
    }

    let total_fields: usize = stdefs.iter().map(|stdef| stdef.fields.len()).sum();
    let Some(mut fields): Option<Ctx<&Ident,&Type>> = stdefs.iter_mut().enumerate()
        .flat_map(|(i,stdef)| stdef.fields.iter_mut().map(move |x| (i,x)))
        .map(|(i,field)| {
            // HACK: changing span allows `..` to pass fields to `foo!(.., arg1, arg2)` from local scope
            let id = field.ident.as_mut()?;
            id.set_span(item_fn.span());
            Some((&*id, (i, &field.ty, None)))
        }).collect() else {
            return syn::Error::new_spanned(
                &stdefs[0],
                "clasma can only partially borrow named structs",
            ).to_compile_error().into();
        };
    {
        let missing_fields = total_fields - fields.len();
        if missing_fields > 0 {
            return syn::Error::new_spanned(
                &borrows,
                format!("Can not partially borrow these structs. There are {missing_fields} overlapping field names."),
            ).to_compile_error().into();
        }
    }

    if let Err(x) = aggregate_accs(borrows.iter().flat_map(|b| b.access.iter()), &mut fields) {
        return x.to_compile_error().into()
    }
    let orig_num_args = item_fn.sig.inputs.len();
    extend_fn_sig(&mut item_fn.sig, &fields);

    let non_borrowed: Vec<_> = (0..orig_num_args).map(|i| format_ident!("_arg{i}")).collect();
    let field_borrow_ix: TokenStream = fields.iter().filter_map(|(&id,&(i,_,borrowed))| {
        let (mu,_) = borrowed?;
        Some(if mu { quote! { #id #i mut } } else { quote! { #id #i } })
    }).collect();
    let func_name = &item_fn.sig.ident;
    let vis = &item_fn.vis;

    let res = quote! {
        #item_fn

        #vis macro #func_name {
            ( < $($lt:lifetime),+ $(, $t:ty)* > , #($#non_borrowed: expr ,)* $($st:tt),* ) => {
                ::clasma::__scoped!({ self::#func_name::< $($lt),* $(,$t)* >( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
            },
            ( < $($t:ty),+ > , #($#non_borrowed: expr ,)* $($st:tt),* ) => {
                ::clasma::__scoped!({ self::#func_name::< $($t),* >( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
            },
            ( #($#non_borrowed: expr ,)* $($st:tt),* ) => {
                ::clasma::__scoped!({ self::#func_name( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
            },
        }
    };
    return res.into();
}

#[proc_macro]
pub fn __clasma_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (stdefs, imports, mut item_impl) = match (|input: ParseStream| {
        let stdefs = {
            let content; braced!(content in input);
            Punctuated::<ItemStruct, token::Semi>::parse_terminated(&content)?
        };
        let imports = {
            let content; bracketed!(content in input);
            Punctuated::<Path, token::Comma>::parse_terminated(&content)?
        };
        let item_impl: ItemImpl = input.parse()?;
        return Ok((stdefs, imports, item_impl))
    }).parse(input) {
        Ok(x) => x,
        Err(x) => return x.into_compile_error().into()
    };
    let stdefs: HashMap<_,_> = imports.iter().zip(stdefs.iter()).collect();

    let Some(st_path) = ('blk: {
        let Type::Path(st_path) = &*item_impl.self_ty else { break 'blk None };
        let mut st_path = st_path.clone();
        let Some(st_name) = st_path.path.segments.last_mut() else { break 'blk None };
        st_name.arguments = PathArguments::None;
        Some(st_path)
    }) else {
        return syn::Error::new_spanned(
            &item_impl,
            "clasma only supports `impl` blocks of `path::to::Type`",
        ).to_compile_error().into();
    };


    let macs: Vec<_> = item_impl.items.iter_mut().filter_map(|item| {
        let ImplItem::Fn(item_fn) = item else { return None };
        let i = item_fn.attrs.iter_mut().position(|Attribute {meta, ..}| {
            let Meta::List(MetaList {path,..}) = meta else { return false };
            if path.segments.last().unwrap().ident != "clasma" { return false };
            true
        })?;
        let Meta::List(MetaList {tokens,..}) = item_fn.attrs.remove(i).meta else { unreachable!() };
        let mut borrows = match Punctuated::<BorrowAttr,token::Comma>::parse_terminated.parse2(tokens) {
            Ok(x) => x, Err(x) => return x.to_compile_error().into(),
        };
        let borrow_gens: Vec<_> = borrows.iter_mut().map(|b| {
            let PathArguments::AngleBracketed(gens) = mem::take(&mut b.path.segments.last_mut()?.arguments) else { return None };
            Some(gens.args)
        }).collect();

        let total_fields: usize = borrows.iter().map(|b| stdefs[&b.path].fields.len()).sum();
        let Some(mut fields): Option<Ctx<Ident,Type>> = borrows.iter().enumerate()
            .flat_map(|(i,b)| stdefs[&b.path].fields.iter().map(move |x| (i,x)))
            .map(|(i,field)| {
                // HACK: changing span allows `..` to pass fields to `foo!(.., arg1, arg2)` from local scope
                // this differs from `clasma_fn`, in that `clasma_fn` may modify
                // its imported struct field types, whereas functions in `impl`
                // share their imports.
                let id = Ident::new(&field.ident.as_ref()?.to_string(), item_fn.span());
                let ty = field.ty.clone();
                Some((id, (i, ty, None)))
            }).collect() else {
                return syn::Error::new_spanned(
                    &borrows,
                    "clasma can only partially borrow named structs",
                ).to_compile_error().into();
            };

        {
            let missing_fields = total_fields - fields.len();
            if missing_fields > 0 {
                return syn::Error::new_spanned(
                    &borrows,
                    format!("Can not partially borrow these structs. There are {missing_fields} overlapping field names."),
                ).to_compile_error().into();
            }
        }

        for (borrow, gens) in borrows.iter().zip(borrow_gens.iter()) {
            let Some(gens) = gens else { continue };
            let mut subst = GenericSubstitutor::new();
            mk_subst_maps(stdefs[&borrow.path].generics.params.iter(), gens.iter(), &mut subst);
            for (_,t,_) in fields.values_mut() {
                subst.visit_type_mut(t);
            }
        }

        if let Err(x) = aggregate_accs(borrows.iter().flat_map(|b| b.access.iter()), &mut fields) {
            return x.to_compile_error().into()
        }

        let orig_num_args = item_fn.sig.inputs.len();
        extend_fn_sig(&mut item_fn.sig, &fields);
        let non_borrowed: Vec<_> = (0..orig_num_args).map(|i| format_ident!("_arg{i}")).collect();
        let field_borrow_ix: TokenStream = fields.iter().filter_map(|(id,&(i,_,borrowed))| {
            let (mu,_) = borrowed?;
            Some(if mu { quote! { #id #i mut } } else { quote! { #id #i } })
        }).collect();

        let func_name = &item_fn.sig.ident;
        let vis = &item_fn.vis;
        let mac = format_ident!("__{func_name}_1");

        let res = quote! {
            #vis macro #func_name {
                ( < $($lt:lifetime),+ $(, $t:ty)* >:: $($rest:tt)* ) => {
                    #mac!( [$($lt),* $(,$t)*] $($rest)* )
                },
                ( < $($t:ty),+ >:: $($rest:tt)* ) => {
                    #mac!( [$($t),*] $($rest)* )
                },
                ( #($#non_borrowed: expr ,)* $($st:tt),* ) => {
                    ::clasma::__scoped!({ self::#st_path::#func_name( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
                },
                ( $($rest:tt)* ) => {
                    #mac!( [] $($rest)* )
                },
            }
            #[doc(hidden)]
            #vis macro #mac {
                ( [$($stgen:tt)*] < $($lt:lifetime),+ $(, $t:ty)* >, #($#non_borrowed: expr ,)* $($st:tt),+ ) => {
                    ::clasma::__scoped!({ self::#st_path::< $($stgen)* >::#func_name::< $($lt,)* $(,$t)* >( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
                },
                ( [$($stgen:tt)*] < $($t:ty),* >, #($#non_borrowed: expr ,)* $($st:tt),+ ) => {
                    ::clasma::__scoped!({ self::#st_path::< $($stgen)* >::#func_name::< $($t),* >( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
                },
                ( [$($stgen:tt)*] , #($#non_borrowed: expr ,)* $($st:tt),+ ) => {
                    ::clasma::__scoped!({ self::#st_path::< $($stgen)* >::#func_name( #($#non_borrowed),* )} [$( {$st} )*] [#field_borrow_ix] )
                },
            }
        };
        Some(res)
    }).collect();

    let res = quote! {
        #item_impl

        #(#macs)*
    };
    return res.into();
}

#[proc_macro_attribute]
pub fn clasma(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as Item);
    match item {
        Item::Struct(item_struct) => {
            let st_ident = &item_struct.ident;
            let vis = &item_struct.vis;
            let res = quote! {
                #item_struct

                #vis macro #st_ident {
                    ( $cb:path { $($defs:tt)* } [] $($extra:tt)* ) => {
                        $cb!( { $($defs)* #item_struct ; } $($extra)* );
                    },
                    ( $cb:path { $($defs:tt)* } [ $p:path $(, $ps:path)* ] $($extra:tt)* ) => {
                        $p!( $cb { $($defs)* #item_struct ; } [ $($ps),* ] $($extra)* );
                    },
                }
            };
            return res.into();
        },
        Item::Fn(item_fn) => {
            let borrows = match Punctuated::<BorrowAttr,token::Comma>::parse_terminated.parse(attr) {
                Ok(x) => x, Err(x) => return x.to_compile_error().into(),
            };
            let mut imports = borrows.iter().map(|b| {
                let mut path = b.path.clone();
                path.segments.last_mut().unwrap().arguments = PathArguments::None;
                path
            });
            let Some(fst_import) = imports.next() else {
                return syn::Error::new_spanned(
                    borrows,
                    "`clasma` attribute must specify its partial borrows: `#[clasma(&<...> path::to::Struct, ...)]`",
                ).to_compile_error().into()
            };
            let res = quote! {
                #fst_import!( ::clasma::__clasma_fn {} [ #(#imports),* ] [#borrows] #item_fn);
            };
            return res.into();
        },
        Item::Impl(item_impl) => {
            if !attr.is_empty() {
                return syn::Error::new_spanned(
                    TokenStream::from(attr),
                    "`#[clasma]` attribute on `impl` block does not take any parameters. Specify partial borrows on each function instead.",
                ).to_compile_error().into()
            }
            let borrows: Result<Vec<_>,_> = item_impl.items.iter().filter_map(|item| {
                let ImplItem::Fn(item_fn) = item else { return None };
                let attr = item_fn.attrs.iter().find_map(|attr| {
                    let Meta::List(attr) = &attr.meta else { return None };
                    if attr.path.segments.last()?.ident != "clasma" { return None };
                    Some(&attr.tokens)
                })?;
                Some(Punctuated::<BorrowAttr,token::Comma>::parse_terminated.parse2(attr.clone()))
            }).collect();
            let borrows = match borrows {Ok(x) => x, Err(x) => return x.into_compile_error().into() };

            let imports_uniq = borrows.iter().flat_map(|bs| bs.iter()).map(|b| {
                let mut path = b.path.clone();
                path.segments.last_mut().unwrap().arguments = PathArguments::None;
                path
            }).collect::<HashSet<_>>();
            let imports = imports_uniq.iter();
            let mut imports_rest = imports_uniq.iter();

            let Some(fst_import) = imports_rest.next() else {
                return syn::Error::new_spanned(
                    item_impl,
                    "`#[clasma]`-annotated `impl` block must contain a function annotated with `#[clasma(&<..> path::to::Struct, ..)]`",
                ).to_compile_error().into()
            };

            let res = quote! {
                #fst_import!( ::clasma::__clasma_impl {} [ #(#imports_rest),* ] [ #(#imports),* ] #item_impl);
            };
            return res.into();
        },
        _ => {
            return syn::Error::new_spanned(
                item,
                "#[clasma] must attribute a struct definition, function definition or `impl` block",
            ).to_compile_error().into();
        },
    }
}


#[test]
fn test_simple_field_access() {
    let input: TokenStream = quote! {
        <field1, field2>: some::path::to::Type
    };

    let parsed: BorrowAttr = syn::parse2(input).expect("Failed to parse");

    assert_eq!(parsed.access.len(), 2);
    let first_access = &parsed.access[0];
    if let FieldAccess::Field(lt, mu, ident) = first_access {
        assert!(lt.is_none());
        assert!(!mu);
        assert_eq!(ident, "field1");
    } else {
        panic!("Expected FieldAccess::Field");
    }

    let path = &parsed.path;
    let path_str = quote!(#path).to_string();
    assert_eq!(path_str, "some :: path :: to :: Type");
}
