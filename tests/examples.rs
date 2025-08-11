// #![no_implicit_prelude]
use ::clasma::{clasma, partial};

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

#[partial(Mystruct)]
fn foo1(a: &mut A, b: &B, some_arg: u8) {
    // ...
}

#[test]
fn ex1() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo1!(mystruct, 3);
    // expands to:
    // foo(&mut mystruct.a, &mystruct.b, 3);
}


#[partial(Mystruct)]
fn foo2<T>(b: &B, some_arg: T, a: &mut A, c: &C) {
    // ...
}

#[test]
fn ex2() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo2!(<&str>, mystruct, "hello");
    // expands to:
    // foo::<&str>(&mystruct.b, "hello", &mut mystruct.a, &mystruct.c);
}

#[partial(Mystruct)]
fn foo3<'t: 'static>(b: &'t B, a: &'t A, other_arg: &'t u8) {
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

    foo3!(<'static>, mystruct, &3);
    // expands to:
    // foo::<'static>(&mystruct.b, &mystruct.a, &3);
}

#[partial(Mystruct)]
// #[clasma(a,b,c)]
fn foo4<T>(a: &A, other_arg: T, c: &mut C) {
    // ...
}

#[partial(Mystruct)]
// #[clasma(a,b,c)]
fn bar4(c: &mut C, some_arg: u8, b: &B, a: &mut A) {

    foo4_scope!(<u8>, some_arg); // supplies the fields of `Mystruct` from local scope
    // expands to:
    // foo::<u8>(a, some_arg, c)
}

#[partial(Mystruct)]
impl Mystruct {
    // ...
    fn foo5(a: &mut A, b: &B, some_arg: u8) {
        // ...
    }

    fn bar5(c: &mut C, some_arg: u8, b: &B, a: &mut A) {

        foo5_scope!(some_arg);
        // expands to:
        // Mystruct::foo::<u8>(a, b, some_arg)
    }
}

#[test]
fn ex5() {
    let mut mystruct = Mystruct {
        a: A::new(),
        b: B::new(),
        c: C::new(),
    };

    foo5!(mystruct, 3);
    // expands to:
    // Mystruct::foo(&mut mystruct.a, &mystruct.b, 3);
}

struct Mystruct6<T> {
    a: A,
    b: B,
    c: T,
}

#[partial(Mystruct)]
impl<T> Mystruct6<T> {
    // ...
    fn foo6<U>(a: &mut A, b: &B, some_arg: U, other_arg: T) {
        // ...
    }
}

#[test]
fn ex6() {
    let mut mystruct = Mystruct6::<&str> {
        a: A::new(),
        b: B::new(),
        c: "hey",
    };

    foo6!(<&str>::<u8>, mystruct, 3, "hello");
    // expands to:
    // Mystruct::<&str>::foo::<u8>(&mut mystruct.a, &mystruct.b, 3, "hello");
}
