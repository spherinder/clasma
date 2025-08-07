# Clasma

<!-- [![Crates.io](https://img.shields.io/crates/v/clasma.svg)](https://crates.io/crates/clasma) -->
<!-- [![Docs.rs](https://docs.rs/clasma/badge.svg)](https://docs.rs/clasma) -->

A procedural attribute macro to reduce boilerplate when passing partially borrow structs into functions.

## Overview

The `#[partial]` function annotation generates a `macro_rules!` macro of the same name and does not touch the function signature. This macro is callable similarly to the original function, except that one can pass a struct instance to provide the arguments annotated with `#[clasma]`.

This attempts to make partial or "split" borrows more ergonomic in Rust, where the borrow checker sometimes requires functions to borrow references to individual fields rather than a single mutable reference to the parent struct, leading to verbose call sites.

## Usage

```rust
struct Mystruct {
    a: A,
    b: B,
}

let mut x = Mystruct {
    a: A::new(),
    b: B::new(),
};
```

The first argument to `foo!` is the struct to be partially borrowed. One passes the remaining arguments without `#[clasma]` attributes in the same order as the signature.

```rust
#[partial]
fn foo(#[clasma] a: &mut A, #[clasma] b: &B, some_arg: u8) {
    // ...
}

foo!(x, 3);
// expands to:
// foo(&mut x.a, &x.b, 3);
```

One can reorder struct fields, as long as field names match up with `#[clasma]` argument names. One can also optionally provide generic parameters inside angle brackets `<...>`.
```rust
#[partial]
fn bar<T>(some_arg: T, #[clasma] b: &B, #[clasma] a: &mut A) {
    // ...
}

bar!(<&str>, x, "hello");
// expands to:
// bar::<&str>("hello", &x.b, &mut x.a);
```

Lifetime parameters are also supported.
```rust
#[partial]
fn baz<'t: 'static>(#[clasma] b: &'t B, #[clasma] a: &'t A, other_arg: &'t u8) {
    // ...
}

const y: Mystruct = Mystruct {
    a: A::new(),
    b: B::new(),
};

baz!(<'static>, y, &3);
// expands to:
// baz::<'static>(&y.b, &y.a, &3);
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
            n_visits += 1;
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
#[partial]
fn visit(#[clasma] destinations: &Vec<String>, #[clasma] n_visits: &mut u32, dest: &String) {
    if destinations.contains(dest) {
        *n_visits += 1;
    }
}

impl Tourist {
    fn visit_all(&mut self) {
        for dest in &self.destinations {
            visit!(self, dest); // `visit!` automatically borrows the corresponding fields of `self`
        }
        println!("Visited {} countries", self.destinations.len());
    }
}
```


## Installation

``` sh
cargo add clasma
```


