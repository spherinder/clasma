<img width="1280" alt="clasma" src="https://github.com/spherinder/clasma/blob/master/assets/clasma-logo.png?raw=true" />

[![Crates.io](https://img.shields.io/crates/v/clasma.svg)](https://crates.io/crates/clasma)
[![Docs.rs](https://docs.rs/clasma/badge.svg)](https://docs.rs/clasma)

A procedural macro library in Rust, that makes **partial borrowing** (splitting borrows) more ergonomic. It reduces boilerplate required to pass individual fields of a struct to a function to satisfy the borrow checker.

## Overview

<table>
<tr>
<th>Write:</th>
<th>Expands it to:</th>
</tr>
<tr>
<td>

```rust
#[clasma(&<mut a, b> MyStruct)]
fn foo(x: i32) {
    // ...
}

// Call via generated macro
foo!(10, my_struct);
```

</td>
<td>

```rust
// Signature accepts explicit fields
fn foo(x: i32, a: &mut A, b: &B) {
    // ...
}

// Macro splits the borrow
foo(10, &mut my_struct.a, &my_struct.b);
```

</td>
</tr>
</table>

## Installation

```sh
cargo add clasma
```

> [!IMPORTANT]  
> **Requirement:** `clasma` needs `#[feature(decl_macro)]`.  
> This ensures that `path::to::foo!` expands hygienically to `path::to::foo`.

## The Problem of Partial Borrows

In Rust, one can not borrow a struct mutably, if it is already borrowed immutably. When a function only needs to access specific fields, but the signature borrows `&mut self`, the borrow checker will unnecessarily complain.

### Example

Imagine a `Tourist` who tracks how many times they visit a destination in his list of destinations.

```rust
struct Tourist {
    n_visits: u32,
    destinations: Vec<String>
}
```

This example code for visiting all destinations once, would fail to compile:

```rust
impl Tourist {
    fn visit(&mut self, dest: &String) {
        if self.destinations.contains(dest) {
            self.n_visits += 1;
        }
    }

    fn visit_all(&mut self) {
        for dest in &self.destinations { // <--- Immutable borrow of `self.destinations`
            self.visit(dest);            // <--- ERROR: Tries to borrow `*self` mutably
        }
    }
}
```

To fix this manually, one must "explode" the struct and pass fields individually:
```rust
fn visit(destinations: &Vec<String>, n_visits: &mut u32, dest: &String) { ... }
```
This becomes tedious and verbose as the number of fields grows.

## Solution

`clasma` automates the "explosion" of the struct into its component fields.

```rust
#[clasma] // 1. Mark the struct
struct Tourist { ... }

#[clasma] // 2. Mark the impl
impl Tourist {
    // 3. Define which fields are needed
    #[clasma(&<destinations, mut n_visits> Tourist)]
    fn visit(dest: &String) {
        if destinations.contains(dest) {
            *n_visits += 1;
        }
    }

    fn visit_all(&mut self) {
        for dest in &self.destinations {
            // 4. Use the generated macro to call the function
            visit!(self, dest); 
        }
    }
}
```

## How it Works

When one attaches `#[clasma(&<...> StructName)]` to a function:

1.  **Signature Transformation:** The macro extends the function signature with the specific fields defined in `<...>` as references.
2.  **Macro Generation:** It creates a macro with the same name as the function (e.g., `visit!`).
3.  **Call Site Desugaring:** When one calls `visit!(..., instance)`, the macro splits `instance` into `&instance.field1`, `&mut instance.field2`, etc., and passes them to the transformed function.

### Syntax

The syntax inside the attribute is: `#[clasma(&< SPECIFIERS > StructName)]`.

| Specifier | Description | Example |
| :--- | :--- | :--- |
| `field` | Immutable borrow | `&<x, y> Point` |
| `mut field` | Mutable borrow | `&<mut x> Point` |
| `'a field` | Lifetime annotation | `&<'a x> Point` |
| `*` | Wildcard (All fields) | `&<*> Point` |
| `!field` | Exclusion (All except...) | `&<*, !z> Point` |

## Advanced Usage

### 1. Generics

Clasma supports generic functions and structs. One can pass generic type parameters to the generated macro.

```rust
#[clasma]
struct Container<T> { item: T }

#[clasma(&<item> Container<T>)]
fn print_item<T: Display>() {
    println!("{}", item);
}

// This will call with turbofish `::<u32>` inside the macro
print_item!(<u32>, 42, my_container); 
```

### 2. Lifetimes

One can specify lifetimes in the attribute, which will be applied to the field references.

```rust
#[clasma(&<'a x, 'a y> Point)]
fn calculate<'a>() { ... }
```

### 3. Argument Propagation from Scope (`..`)

Inside a function, where the fields are *already* available as bound variables (e.g., inside another `clasma`-annotated function), one can use `..` instead of a struct instance. `clasma` will match the field names from local scope.

```rust
#[clasma(&<x, mut y> Point)]
fn inner() { ... }

#[clasma(&<*> Point)] // Adds x, y, z as arguments to `outer`
fn outer() {
    // 'x' and 'y' are already in scope here.
    // '..' tells clasma to find them locally.
    inner!(..); 
}
```

### 4. Multiple Structs

One can partially borrow from multiple structs in a single function signature.

```rust
#[clasma(&<mut a> StructA, &<b, c> StructB)]
fn mix_and_match() { ... }

mix_and_match!(struct_a_instance, struct_b_instance);
```

### 5. `impl` Blocks

When using `clasma` inside an `impl` block, the generated macro is namespaced.

```rust
#[clasma]
impl MyStruct {
    #[clasma(&<a, b> MyStruct)]
    fn helper() { ... }
    
    fn main(&self) {
        // Calls MyStruct::helper(self.a, self.b)
        helper!(self);
    }
}
```

If both the type and the function are generic, use `::` to separate them in the macro call:
```rust
foo!(<TypeParams>::<FnParams>, ...Args... , instance);
```

---

## References and Background

This crate tries to address a long-standing "papercut" in Rust:
*   [Interprocedural Conflicts](https://smallcultfollowing.com/babysteps/blog/2018/11/01/after-nll-interprocedural-conflicts/)
*   [Rust Internals: Notes on partial borrows](https://internals.rust-lang.org/t/notes-on-partial-borrows/20020)
*   [The Rustonomicon: Splitting Borrows](https://doc.rust-lang.org/nomicon/borrow-splitting.html)
*   [RFC Issue: Partial borrows](https://github.com/rust-lang/rfcs/issues/1215)
