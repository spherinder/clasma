> [!CAUTION]
> **Deprecated:** No longer maintained. Use [borrow](https://crates.io/crates/borrow) instead.


<img width="1280" alt="clasma" src="https://github.com/spherinder/clasma/blob/master/assets/clasma-logo.png?raw=true" />

[![Crates.io](https://img.shields.io/crates/v/clasma.svg)](https://crates.io/crates/clasma)
[![Docs.rs](https://docs.rs/clasma/badge.svg)](https://docs.rs/clasma)

A procedural attribute macro to reduce boilerplate when writing functions that partially borrow a struct.

## Overview

This crate attempts to make partial or "split" borrows[^1][^2][^3][^4][^5][^6] more ergonomic in Rust – a problem where functions sometimes must borrow individual fields, rather than a single mutable reference to a struct, in order to adhere to borrowing rules, leading to verbose call sites.

With a struct definition like
```rust
#[clasma]
struct Mystruct { ... }
```
adding `#[clasma(&<...> Mystruct)]` to a function definition `fn foo(..) {}`
1. extends `foo`'s arguments with borrows of some of `Mystruct`'s fields specified in `&<...>` and
2. generates a macro `foo!` – essentially callable just like `foo`'s original signature but taking an additional an instance `mystruct` of `Mystruct` to partially borrow
```rust
#[clasma(&<...> Mystruct)]
fn foo( ... ) { ... }

foo!( ... , mystruct);
```
expands to
```rust
fn foo( ..., <field>: &<field_type>, <field>: &<field_type>... ) { ... }

foo( ... , &mystruct.<field>, &mystruct.<field>);
```
where, again, the fields are specified between `&<...>`.

[^1]: [Rust Internals "Notes on partial borrow"](https://internals.rust-lang.org/t/notes-on-partial-borrows/20020).
[^2]: [The Rustonomicon "Splitting Borrows"](https://doc.rust-lang.org/nomicon/borrow-splitting.html).
[^3]: [Niko Matsakis Blog Post "After NLL: Interprocedural conflicts"](https://smallcultfollowing.com/babysteps/blog/2018/11/01/after-nll-interprocedural-conflicts/).
[^4]: [Afternoon Rusting "Multiple Mutable References"](https://oribenshir.github.io/afternoon_rusting/blog/mutable-reference).
[^5]: [Partial borrows Rust RFC](https://github.com/rust-lang/rfcs/issues/1215#issuecomment-333316998).
[^6]: [HackMD "My thoughts on (and need for) partial borrows"](https://hackmd.io/J5aGp1ptT46lqLmPVVOxzg?view).

## Installation

> [!IMPORTANT]
> `clasma` requires `#[feature(decl_macro)]`
> 
> This is in order to ensure, that `path::to::foo!` expands to `path::to::foo` as opposed to just `foo`. `macro_rules!` macros are unhygienic in this regard.

```sh
cargo add clasma
```

## Motivating Example

Imagine a `Tourist` with a list of travel destinations, that counts the number of times he visits one of his destinations.

``` rust
struct Tourist {
    n_visits: u32,
    destinations: Vec<String>
}
```

### Before

Without partial borrowing, this code does not compile.

```rust
impl Tourist {
    fn visit(&mut self, dest: &String) {
        if self.destinations.contains(dest) {
            self.n_visits += 1;
        }
    }

    fn visit_all(&mut self) {
        for dest in &self.destinations { // <- immutable borrow occurs here
            self.visit(dest); // ERROR: `*self` is also borrowed as immutable 
        }
        println!("Visited {} countries", self.destinations.len());
    }
}
```

The issue is that `visit` unnecessarily borrows `self.destinations` mutably when it only needs an immutable borrow. A workaround would be to borrow each struct field separately on every call to `visit`:

```rust
impl Tourist {
    fn visit(destinations: &Vec<String>, n_visits: &mut u32, dest: &String) {
        if destinations.contains(dest) {
            *n_visits += 1;
        }
    }

    fn visit_all(&mut self) {
        for dest in &self.destinations {
            Self::visit(&self.destinations, &mut self.n_visits, dest);
        }
        println!("Visited {} countries", self.destinations.len());
    }
}
```

However, one can imagine how tedious it would get, as the number of fields increases.

### With `clasma`

The `clasma` macro handles borrowing and argument passing.

```rust
#[clasma]
struct Tourist { ... }

#[clasma]
impl Tourist {
    #[clasma(&<destinations, mut n_visits> Tourist)]
    fn visit(dest: &String) {
        if destinations.contains(dest) {
            *n_visits += 1;
        }
    }
    // ...
    fn visit_all(&mut self) {
        for dest in &self.destinations {
            visit!(self, dest); // Now calls to `visit` become more concise
        }
        println!("Visited {} countries", self.destinations.len());
    }
}
```

## Usage

Consider the following struct:
```rust
#[clasma]
struct Mystruct {
    a: A,
    b: B,
    c: C,
}

let mut mystruct = Mystruct {
    a: A::new(),
    b: B::new(),
    c: C::new(),
};
```

The `#[clasma(&<...> Mystruct)]` attribute proc-macro expands `foo`'s signature with the fields specified between `&<...>`. For example,

```rust
#[clasma(&<mut a, b> Mystruct)]
fn foo(some_arg: u8) {
    // ...
}
```
expands to
```rust
fn foo(some_arg: u8, a: &mut A, b: &B) {
    // ...
}
```

Additionally, `#[clasma(&<...> Mystruct)]` generates a macro `foo!`. The first arguments to `foo!` are the arguments of `foo`'s original signature (`some_arg: u8`) and the final argument is an instance of `Mystruct`. `foo!(..., mystruct)` borrows each of `mystruct`'s fields individually, passing them into `foo`'s corresponding arguments.
```rust
foo!(3, mystruct);
```
expands to
```rust
foo(3, &mut mystruct.a, &mystruct.b);
```

One can also optionally provide generic type parameters inside angle brackets `<...>`:
```rust
#[clasma(&<a, b> Mystruct)]
fn foo<T>(some_arg: T) {
    // ...
}

foo!(<&str>, "hello", mystruct);
```
expands to
```rust
fn foo<T>(some_arg: T, a: &A, b: &B) {
    // ...
}

foo::<&str>("hello", &mystruct.a, &mystruct.b);
```

`#[clasma(&<...> Mystruct)]` supports adding lifetime annotations to partially borrowed fields in `&<...>`, and one can likewise call `foo!` with lifetime parameters:

```rust
const mystruct: Mystruct = Mystruct { ... };

#[clasma(&<'t a, 't b> Mystruct)]
fn foo<'t: 'static, T>(other_arg: &'t u8) {
    // ...
}

foo!(<'static>, &3, mystruct);
```
expands to
```rust
const mystruct: Mystruct = Mystruct { ... };

fn foo<'t: 'static, T>(other_arg: &'t u8, a: &'t A, b: &'t B) {
    // ...
}

foo::<'static>(&3, &mystruct.a, &mystruct.b);
```

In addition to enumerating field names in `&<...>` manually, one can also use a wildcard `*` to specify all fields, and `!` to exclude a field.

For example, `&<'t mut a, 's *, mut b, !c> Mystruct` is equivalent to `&<'s a, 's b> Mystruct`.

### Generic structs

``` rust
#[clasma]
struct Mystruct<T,U> {
    a: Vec<T>,
    b: Vec<(T,U)>,
    c: U,
}

#[clasma(&<*> Mystruct<T, bool>)]
fn foo<U>(some_arg: U) {
    // ...
}

let mystruct = Mystruct {
    a: vec!["hi"],
    b: vec![("bye", false)],
    c: true,
}

foo!(<&str>, "hello", mystruct);
```
expands to
``` rust
struct Mystruct<T,U> { ... }

fn foo<U>(some_arg: T, a: &Vec<U>, b: &Vec<(U,bool)>, c: &bool) {
    // ...
}

let mystruct = Mystruct {
    a: vec!["hi"],
    b: vec![("bye", false)],
    c: true,
}

foo::<&str>("hello", &mystruct.a, &mystruct.b, &mystruct.c);
```

### Propagating from scope

In addition to partially borrowing fields from a struct instance, `foo!` also accepts `..` in place of the struct instance. The arguments to `foo` previously supplied by `mystruct`, are now read directly from the local scope at the call site.

```rust
#[clasma(&<a, mut c> Mystruct)]
fn foo<T>(some_arg: T) {
    // ...
}

#[clasma(&<*> Mystruct)]
fn bar() {
    let c = &mut C::new();
    foo!(<u8>, 3, ..); // supplies the fields of `Mystruct` from local scope
}
```
expands to
```rust
fn foo<T>(some_arg: T, a: &A, c: &mut C) {
    // ...
}

fn bar(a: &A, b: &B, c: &C) {
    let c = &mut C::new();
    foo::<u8>(3, a, c);
}
```

### Partially borrowing several structs

Presuming that two structs have non-overlapping fields, one can partially borrow both structs with `#[clasma(&<...> Struct1, &<...> Struct2)]`:

```rust
#[clasma]
struct Other {
    o1: A,
    o2: B,
    o3: C,
}

#[clasma(&<a, mut c> Mystruct, &<mut o2, o3> Other)]
fn foo<T>(some_arg: T) {
    // ...
}

let a = &A();
let c = &mut C();
let mut other = Other {
    o1: A::new(),
    o2: B::new(),
    o3: C::new(),
};

foo!(<u8>, 3, .., other); // supplies the fields of `Mystruct` locally and fields of `Other` from `other`
```
expands to
``` rust
struct Other { ... }

fn foo<T>(some_arg: T, a: &A, c: &mut C, o2: &mut B, o3: &C) {
    // ...
}

let a = &A();
let c = &mut C();
let mut other = Other {
    o1: A::new(),
    o2: B::new(),
    o3: C::new(),
};

foo::<u8>(3, a, c, &mut other.o2, &other.o3);
```

### `impl` blocks

For `impl` blocks, a `#[clasma]` attribute must go on top of the `impl`. One can then annotate inner functions with `#[clasma(&<...> Mystruct)]` like usual:
``` rust
#[clasma]
impl Mystruct {
    // ...
    #[clasma(&<mut a, b> Mystruct)]
    fn foo(some_arg: u8) {
        // ...
    }

    #[clasma(&<mut *> Mystruct)]
    fn bar(other_arg: u8) {

        foo!(other_arg, ..);
    }
}

foo!(3, mystruct);
```
expands to
``` rust
impl Mystruct {
    // ...
    fn foo(some_arg: u8, a: &mut A, b: &B) {
        // ...
    }

    fn bar(other_arg: u8, a: &mut A, b: &mut B, c: &mut C) {

        Mystruct::foo(other_arg, a, b)
    }
}

Mystruct::foo(3, &mut mystruct.a, &mystruct.b);
```

If *both* the type and the function in the `impl` block are generic, one can provide the arguments separated by `::` like so:
``` rust
#[clasma]
impl<T> Mystruct<T> {
    // ...
    #[clasma(&<mut a, b> Mystruct<T>)]
    fn foo<U>(some_arg: U, other_arg: T) {
        // ...
    }
}

foo!(<&str>::<u8>, 3, "hello", mystruct);
// expands to:
// Mystruct::<&str>::foo::<u8>(3, "hello", &mut mystruct.a, &mystruct.b);
```

If *only* the type is generic, one can omit the second pair of angle brackets and pass the arguments like this:

``` rust
foo!(<T>::, ...)
```

