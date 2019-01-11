use std::fs::File;
use std::io::Read;

mod parser;

pub fn load_image(path: &str) -> Vec<u16> {
    let mut file = File::open(path).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();

    let (_, result) = parser::parse_program(&buffer).unwrap();
    let origin = result.origin as usize;
    let mut memory = vec![0; u16::max_value() as usize];

    let memory_slice = &mut memory[origin..];

    for i in 0..result.program.len() {
        memory_slice[i] = result.program[i];
    }
    memory
}
