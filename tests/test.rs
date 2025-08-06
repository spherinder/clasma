use clasma::partial;

struct A();
struct B();
struct C(u8);

struct Mystruct {
    name1: A,
    name2: B,
    name3: C,
}

#[partial]
fn foo<T>(#[clasma] name2: &B, #[clasma] name1: &mut A, other_arg: T) {}

#[partial]
fn bar(some_arg: u8, #[clasma] name2: &mut B, #[clasma] name3: &C) {}

#[test]
fn t() {
    let mut x = Mystruct {
        name1: A(),
        name2: B(),
        name3: C(3),
    };
    bar!(x, 3);
    // expands to:
    // bar(3, &mut x.name2, &x.name3);
    foo!(<&str>, x, "hello");
    // expands to:
    // foo::<&str>(&x.name2, &mut x.name1, "hello");
}
