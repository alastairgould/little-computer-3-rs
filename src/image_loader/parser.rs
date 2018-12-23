use nom::be_u16;

pub struct Program {
    pub origin: u16,
    pub program: Vec<u16>,
}

named!(pub parse_program(&[u8]) -> Program, 
do_parse!(
    origin: be_u16 >>
    program: many0!(complete!(be_u16)) >>
    (Program {origin: origin, program: program})));
