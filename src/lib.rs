#![feature(label_break_value)]
#![feature(core_intrinsics)]
#![feature(concat_idents)]
#![feature(box_patterns)]
#![feature(type_alias_impl_trait)]
#![feature(move_ref_pattern)]
pub mod arraydep;
pub mod ast;
pub mod gcd;
pub mod graph;
pub mod passes;
//pub mod unionfind;
pub mod frontend;
pub mod helper;
//pub mod fixedmap;
//pub mod linkedlist;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
