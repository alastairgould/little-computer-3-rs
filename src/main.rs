#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate nom;

extern crate num;
#[macro_use]
extern crate num_derive;

use std::env;

mod cpu;
mod image_loader;

fn main() {
    let arguments: Vec<String> = env::args().collect();
    let program_to_load = &arguments[1];

    let program = image_loader::load_image(program_to_load);
    let cpu = cpu::Cpu::new(program);
    cpu.run_program();
}
