<img width="1280" alt="clasma" src="https://github.com/spherinder/clasma/blob/master/assets/clasma-logo.png?raw=true" />

[![Crates.io](https://img.shields.io/crates/v/clasma.svg)](https://crates.io/crates/clasma)
[![Docs.rs](https://docs.rs/clasma/badge.svg)](https://docs.rs/clasma)

A procedural attribute macro to reduce boilerplate when writing functions that partially borrow a struct.

## Overview
Annotating a struct `Mystruct` with `#[clasma]`, allows the attribute proc-macro `#[partial(Mystruct)]` on a function `foo`, to generate a macro `foo!`, that is callable similarly to `foo`, except that one can pass an instance of `Mystruct` to provide the arguments in `foo` that correspond to `Mystruct`'s fields.

This crate attempts to make partial or "split" borrows more ergonomic in Rust, where functions sometimes must borrow individual fields, rather than a single mutable reference to a struct, in order to adhere to borrowing rules, leading to verbose call sites.

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

The `clasma::partial` attribute proc-macro generates a macro `foo!`. The first argument to the `foo!` is `mystruct`. `foo!` borrows each of `mystruct`'s fields individually, matching `foo`'s argument names. One passes the remaining arguments in the same order as the signature.

```rust
#[partial(Mystruct)]
fn foo(a: &mut A, b: &B, some_arg: u8) {
    // ...
}

foo!(mystruct, 3);
// expands to:
// foo(&mut mystruct.a, &mystruct.b, 3);
```

One can reorder arguments; they just have to match with the struct's fields. One can also optionally provide generic parameters inside angle brackets `<...>`.
```rust
#[partial(Mystruct)]
fn foo<T>(b: &B, some_arg: T, a: &mut A, c: &C) {
    // ...
}

foo!(<&str>, mystruct, "hello");
// expands to:
// foo::<&str>(&mystruct.b, "hello", &mut mystruct.a, &mystruct.c);
```

Lifetime parameters are also supported.
```rust
#[partial(Mystruct)]
fn foo<'t: 'static>(b: &'t B, a: &'t A, other_arg: &'t u8) {
    // ...
}

const mystruct: Mystruct = Mystruct { ... };

foo!(<'static>, mystruct, &3);
// expands to:
// foo::<'static>(&mystruct.b, &mystruct.a, &3);
```

### Propagating from scope

Two functions marked with identical `clasma::partial` attributes, can call one another using `<function name>_scope!` macros. One calls `foo_scope!` identically to `foo!`, except that one omits the `mystruct`. The arguments to `foo` previously supplied by `mystruct`, now come directly from the local scope at the call site.

```rust
#[partial(Mystruct)]
fn foo<T>(a: &A, other_arg: T, c: &mut C) {
    // ...
}

#[partial(Mystruct)]
fn bar(c: &mut C, some_arg: u8, b: &B, a: &mut A) {
    
    foo_scope!(<u8>, some_arg); // supplies the fields of `Mystruct` from local scope
    // expands to:
    // foo::<u8>(a, some_arg, c)
}
```

### `impl`-blocks

For `impl` blocks, the `#[partial(..)]` attribute goes on top of the `impl`. This will generate macros for all functions in the block.

``` rust
#[partial(Mystruct)]
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
#[partial(Mystruct)]
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
#[clasma]
struct Tourist {...}

#[partial(Tourist)]
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
