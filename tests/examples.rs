#![feature(trace_macros)]
#![feature(decl_macro)]
#![allow(unused)]
// trace_macros!(true);
use clasma::clasma;

struct A();
impl A { const fn new() -> Self { Self() } }
struct B();
impl B { const fn new() -> Self { Self() } }
struct C();
impl C { const fn new() -> Self { Self() } }

#[clasma]
struct Mystruct {
    a: A,
    b: B,
    c: C,
}
#[clasma]
struct Other {
    o1: A,
    o2: B,
    o3: C,
}

#[clasma(&<mut a, b> Mystruct)]
fn foo1(some_arg: u8) {
    // ...
}

#[test]
fn ex1() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo1!(3, mystruct);
    // expands to:
    // foo(&mut mystruct.a, &mystruct.b, 3);
}


#[clasma(&<*, mut a> Mystruct)]
fn foo2<T>(some_arg: T) {
    // ...
}

#[test]
fn ex2() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo2!("hello", mystruct);
    // expands to:
    // foo::<&str>(&mystruct.b, "hello", &mut mystruct.a, &mystruct.c);
}

#[clasma(&<'t *, !c> Mystruct)]
fn foo3<'t: 'static>(other_arg: &'t u8) {
    // ...
}

#[test]
fn ex3() {
    #[allow(non_upper_case_globals)]
    const mystruct: Mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo3!(<'static>, &3, mystruct);
    // expands to:
    // foo::<'static>(&mystruct.b, &mystruct.a, &3);
}

#[clasma(&<a, mut c> Mystruct)]
fn foo4<T>(other_arg: T) {
    // ...
}

// #[clasma(&<mut a, b, mut c> Mystruct)]
fn bar4(some_arg: u8) {
    let a = &A();
    let c = &mut C();
    foo4!(<u8>, some_arg, ..); // supplies the fields of `Mystruct` from local scope
    // expands to:
    // foo::<u8>(a, some_arg, c)
}

#[clasma(&<a, mut c> Mystruct, &<mut o2, o3> Other)]
fn foo5<T>(other_arg: T) {
    // ...
}

// #[clasma(&<mut a, b, mut c> Mystruct)]
fn bar5(some_arg: u8) {
    let a = &A();
    let c = &mut C();
    let mut other = Other {
        o1: A(),
        o2: B(),
        o3: C(),
    };

    foo5!(<u8>, some_arg, .., other); // supplies the fields of `Mystruct` from local scope
    // expands to:
    // foo::<u8>(a, some_arg, c)
}

#[clasma]
impl Mystruct {
    // ...
    #[clasma(&<mut a, b> Mystruct)]
    fn foo6(some_arg: u8) {
        // ...
    }

    fn bar6(c: &mut C, some_arg: u8, b: &B, a: &mut A) {

        foo6!(some_arg, ..);
        // expands to:
        // Mystruct::foo::<u8>(a, b, some_arg)
    }
}

#[test]
fn ex6() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo6!(3, mystruct);
    // expands to:
    // Mystruct::foo(3, &mut mystruct.a, &mystruct.b);
}

#[clasma]
struct Mystruct7<T> {
    a: A,
    b: B,
    c: T,
}

#[clasma]
impl<T> Mystruct7<T> {
    // ...
    #[clasma(&<mut a, b> Mystruct7<T>)]
    fn foo7<U>(some_arg: U, other_arg: T) {
        // ...
    }
}

#[test]
fn ex7() {
    let mut mystruct = Mystruct7::<&str> {
        a: A::new(),
        b: B::new(),
        c: "hey",
    };

    foo7!(<&str>::<u8>, 3, "hello", mystruct);
    // expands to:
    // Mystruct::<&str>::foo::<u8>(&mut mystruct.a, &mystruct.b, 3, "hello");
}
