use num::ToPrimitive;
use std::ops::{Index, IndexMut};

#[derive(FromPrimitive, ToPrimitive)]
pub enum Register {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    RPc,
    RCond,
    RCount,
}

impl Index<Register> for Vec<u16> {
    type Output = u16;

    fn index(&self, register: Register) -> &u16 {
        let index = register.to_usize().unwrap();
        &self[index]
    }
}

impl IndexMut<Register> for Vec<u16> {
    fn index_mut(&mut self, register: Register) -> &mut u16 {
        let index = register.to_usize().unwrap();
        &mut self[index]
    }
}
