<img width="1280" alt="clasma" src="https://github.com/spherinder/clasma/blob/master/assets/clasma-logo.png?raw=true" />

[![Crates.io](https://img.shields.io/crates/v/clasma.svg)](https://crates.io/crates/clasma)
[![Docs.rs](https://docs.rs/clasma/badge.svg)](https://docs.rs/clasma)

A procedural attribute macro to reduce boilerplate when passing partially borrow structs into functions.

## Overview

The `#[clasma(..)]` function annotation generates a `macro_rules!` macro of the same name and does not touch the function signature. This macro is callable similarly to the original function, except that one can pass a struct instance to provide the arguments corresponding to its fields.

This attempts to make partial or "split" borrows more ergonomic in Rust, where the borrow checker sometimes requires functions to borrow references to individual fields rather than a single mutable reference to the parent struct, leading to verbose call sites.

## Usage

Consider the following struct:
```rust
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

By taking fields of `Mystruct` as arguments, the `#[clasma(..)]` attribute on a function `foo` will generate a macro `foo!`. Due to proc-macros being unable to resolve a struct identifier's field names, one **must provide all field names of `Mystruct` to `#[clasma(..)]`**.

The first argument to the `foo!` is an instance of `Mystruct`. `foo!` borrows each of `mystruct`'s fields individually that match `foo`'s argument names. One passes the remaining arguments in the same order as the signature.

```rust
// Must list *all* field names of `Mystruct`
#[clasma(a,b,c)]
fn foo(a: &mut A, b: &B, some_arg: u8) {
    // ...
}

foo!(mystruct, 3);
// expands to:
// foo(&mut mystruct.a, &mystruct.b, 3);
```

One can reorder arguments; they just have to match with the struct's fields. One can also optionally provide generic parameters inside angle brackets `<...>`.
```rust
#[clasma(a,b,c)]
fn foo<T>(b: &B, some_arg: T, a: &mut A, c: &C) {
    // ...
}

foo!(<&str>, mystruct, "hello");
// expands to:
// foo::<&str>(&mystruct.b, "hello", &mut mystruct.a, &mystruct.c);
```

Lifetime parameters are also supported.
```rust
#[clasma(a,b,c)]
fn foo<'t: 'static>(b: &'t B, a: &'t A, other_arg: &'t u8) {
    // ...
}

const mystruct: Mystruct = Mystruct { ... };

foo!(<'static>, mystruct, &3);
// expands to:
// foo::<'static>(&mystruct.b, &mystruct.a, &3);
```

### Propagating from scope

Two functions marked with identical `#[clasma(..)]` attributes can call one another using `<function name>_scope!` macros. One calls `foo_scope!` identically to `foo!`, except that one omits the `mystruct`. The arguments to `foo` previously supplied by `mystruct` now come directly from the local scope at the call site.

```rust
#[clasma(a,b,c)]
fn foo<T>(a: &A, other_arg: T, c: &mut C) {
    // ...
}

#[clasma(a,b,c)]
fn bar(c: &mut C, some_arg: u8, b: &B, a: &mut A) {
    
    foo_scope!(<u8>, some_arg); // supplies the fields of `Mystruct` from local scope
    // expands to:
    // foo::<u8>(a, some_arg, c)
}
```

### `impl`-blocks

For `impl` blocks, the `#[clasma(..)]` attribute goes on top of the `impl`. This will generate macros for all functions in the block.

``` rust
#[clasma(a,b,c)]
impl Mystruct {
    // ...
    fn foo(a: &mut A, b: &B, some_arg: u8) {
        // ...
    }

    fn bar(c: &mut C, some_arg: u8, b: &B, a: &mut A) {

        foo_scope!(some_arg);
        // expands to:
        // Mystruct::foo(a, b, some_arg)
    }
}

foo!(mystruct, 3);
// expands to:
// Mystruct::foo(&mut mystruct.a, &mystruct.b, 3);
```

If *both* the type and the function in the `impl` block are generic, one can provide the arguments separated by `::` like so:
``` rust
#[clasma(a,b,c)]
impl<T> Mystruct<T> {
    // ...
    fn foo<U>(a: &mut A, b: &B, some_arg: U, other_arg: T) {
        // ...
    }
}

foo!(<&str>::<u8>, mystruct, 3, "hello");
// expands to:
// Mystruct::<&str>::foo::<u8>(&mut mystruct.a, &mystruct.b, 3, "hello");
```

If *only* the type is generic, one passes the arguments like this: `foo!(<T>::, mystruct, ...)`

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

However, this gets tedious as the number of fields increases.

### With `clasma::partial`

The `partial` macro handles borrowing and argument passing.

```rust
#[clasma(n_visits, destinations)]
impl Tourist {
    // ...
    fn visit_all(&mut self) {
        for dest in &self.destinations {
            visit!(self, dest); // Now the call to `visit` becomes much more concise
        }
        println!("Visited {} countries", self.destinations.len());
    }
}
```


## Installation

```sh
cargo add clasma
```
