#![allow(unused)]
#![feature(decl_macro)]
#![no_implicit_prelude]
use ::clasma::clasma;

pub struct A();
pub struct B();
pub struct C(u8);

#[clasma]
pub struct Par {
    pub name1: A,
    pub name2: B,
    pub name3: C,
}

#[clasma(&<name1> Par)]
fn parfn() {}

mod sub {
    fn test_from_sub() {
        let ms = super::Par {
            name1: super::A(),
            name2: super::B(),
            name3: super::C(3),
        };
        super::parfn!(ms);
    }

    #[super::clasma]
    pub struct Sub {
        pub name1: super::A,
        pub name2: super::B,
        pub name3: super::C,
    }

    #[super::clasma(&<name1> Sub)]
    pub fn subfn() {}

    #[super::clasma(&<*> super::Par)]
    pub fn subfn2() {}

}

fn test_from_par() {
    let ms = sub::Sub {
        name1: A(),
        name2: B(),
        name3: C(3),
    };
    sub::subfn!(ms);
}
