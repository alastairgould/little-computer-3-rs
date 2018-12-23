use super::registers::Register;
use num::FromPrimitive;

pub type Address = u16;
pub type BinaryInstruction = u16;
pub type MemoryOffset = i16;
pub type ProgramCounterOffset = i16;

bitflags! {
    pub struct ConditionFlag: u16 {
        const Positive = 0b00000001;
        const Zero = 0b00000010;
        const Negative = 0b00000100;
    }
}

pub enum Instruction {
    OpAdd(OpAdd),
    OpLdi(OpLdi),
    OpNot(OpNot),
    OpAnd(OpAnd),
    OpJmp(OpJmp),
    OpBr(OpBr),
    OpJsr(OpJsr),
    OpLoad(OpLoad),
    OpLdr(OpLdr),
    OpLea(OpLea),
    OpSt(OpSt),
    OpSti(OpSti),
    OpStr(OpStr),
    OpTrap(TrapCode),
    OpRti(),
    OpRes(),
}

impl From<u16> for Instruction {
    fn from(binary_instruction: u16) -> Self {
        let op_code = binary_instruction >> 12;

        match op_code {
            0b0000 => Instruction::OpBr(binary_instruction.into()),
            0b0001 => Instruction::OpAdd(binary_instruction.into()),
            0b0101 => Instruction::OpAnd(binary_instruction.into()),
            0b1010 => Instruction::OpLdi(binary_instruction.into()),
            0b1001 => Instruction::OpNot(binary_instruction.into()),
            0b1100 => Instruction::OpJmp(binary_instruction.into()),
            0b0100 => Instruction::OpJsr(binary_instruction.into()),
            0b0010 => Instruction::OpLoad(binary_instruction.into()),
            0b0110 => Instruction::OpLdr(binary_instruction.into()),
            0b1110 => Instruction::OpLea(binary_instruction.into()),
            0b0011 => Instruction::OpSt(binary_instruction.into()),
            0b1011 => Instruction::OpSti(binary_instruction.into()),
            0b0111 => Instruction::OpStr(binary_instruction.into()),
            0b1111 => Instruction::OpTrap(binary_instruction.into()),
            0b1000 => Instruction::OpRti(),
            0b1101 => Instruction::OpRes(),
            _ => panic!("Unsupported op code"),
        }
    }
}

#[derive(FromPrimitive)]
pub enum TrapCode {
    GetChar = 0x20,
    OutChar = 0x21,
    Puts = 0x22,
    Input = 0x23,
    PutByteString = 0x24,
    Halt = 0x25,
}

impl From<u16> for TrapCode {
    fn from(binary_instruction: u16) -> Self {
        let trap_code_byte = binary_instruction & 0b11111111;
        let trap_code = TrapCode::from_u16(trap_code_byte);

        match trap_code {
            Some(trap_code) => trap_code,
            None => panic!("Unsuported trap code"),
        }
    }
}

pub enum OpAdd {
    NormalMode(OpAddNormal),
    ImmediateMode(OpAddImmediate),
}

impl From<u16> for OpAdd {
    fn from(binary_instruction: u16) -> Self {
        let immediate_mode = ((binary_instruction >> 5) & 0b0001) != 0;

        match immediate_mode {
            true => OpAdd::ImmediateMode(OpAddImmediate::from(binary_instruction)),
            false => OpAdd::NormalMode(OpAddNormal::from(binary_instruction)),
        }   
    }
}

pub struct OpAddNormal {
    pub destination_register: Register,
    pub first_value_register: Register,
    pub second_value_register: Register,
}

impl From<u16> for OpAddNormal {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let first_value_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let second_value_register = Register::from_u16(binary_instruction & 0b0111).unwrap();

        OpAddNormal {
            destination_register: destination_register,
            first_value_register: first_value_register,
            second_value_register: second_value_register,
        }
    }
}

pub struct OpAddImmediate {
    pub destination_register: Register,
    pub first_value_register: Register,
    pub second_value: i16,
}

impl From<u16> for OpAddImmediate {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let first_value_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let second_value = sign_extend(binary_instruction & 0b00011111, 5);

        OpAddImmediate {
            destination_register: destination_register,
            first_value_register: first_value_register,
            second_value: convert_to_signed(second_value),
        }
    }
}

pub enum OpAnd {
    NormalMode(OpAndNormal),
    ImmediateMode(OpAndImmediate),
}

impl From<u16> for OpAnd {
    fn from(binary_instruction: u16) -> Self {
        let immediate_mode = ((binary_instruction >> 5) & 0b0001) != 0;

        match immediate_mode {
            true => OpAnd::ImmediateMode(OpAndImmediate::from(binary_instruction)),
            false => OpAnd::NormalMode(OpAndNormal::from(binary_instruction)),
        }
    }
}

pub struct OpAndNormal {
    pub destination_register: Register,
    pub first_value_register: Register,
    pub second_value_register: Register,
}

impl From<u16> for OpAndNormal {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let first_value_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let second_value_register = Register::from_u16(binary_instruction & 0b0111).unwrap();

        OpAndNormal {
            destination_register: destination_register,
            first_value_register: first_value_register,
            second_value_register: second_value_register,
        }
    }
}

pub struct OpAndImmediate {
    pub destination_register: Register,
    pub first_value_register: Register,
    pub second_value: u16,
}

impl From<u16> for OpAndImmediate {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let first_value_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let second_value = sign_extend(binary_instruction & 0b00011111, 5);

        OpAndImmediate {
            destination_register: destination_register,
            first_value_register: first_value_register,
            second_value: second_value,
        }
    }
}

pub struct OpLdi {
    pub destination_register: Register,
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpLdi {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let program_counter_offset = sign_extend(binary_instruction & 0b000111111111, 9);

        OpLdi {
            destination_register: destination_register,
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub struct OpLoad {
    pub destination_register: Register,
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpLoad {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let program_counter_offset = sign_extend(binary_instruction & 0b000111111111, 9);

        OpLoad {
            destination_register: destination_register,
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub struct OpBr {
    pub program_counter_offset: ProgramCounterOffset,
    pub condition_flag: ConditionFlag,
}

impl From<u16> for OpBr {
    fn from(binary_instruction: u16) -> Self {
        let program_counter_offset = sign_extend((binary_instruction) & 0b000111111111, 9);
        let condition_flag = ConditionFlag::from_bits_truncate((binary_instruction >> 9) & 0b0111);

        OpBr {
            program_counter_offset: convert_to_signed(program_counter_offset),
            condition_flag: condition_flag,
        }
    }
}

pub enum OpJsr {
    Offset(OpJsrOffset),
    Register(OpJsrRegister),
}

impl From<u16> for OpJsr {
    fn from(binary_instruction: u16) -> Self {
        let offset_mode = ((binary_instruction >> 11) & 1) != 0;

        match offset_mode {
            true => OpJsr::Offset(OpJsrOffset::from(binary_instruction)),
            false => OpJsr::Register(OpJsrRegister::from(binary_instruction)),
        }
    }
}

pub struct OpJsrOffset {
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpJsrOffset {
    fn from(binary_instruction: u16) -> Self {
        let program_counter_offset = sign_extend((binary_instruction) & 0b011111111111, 11);

        OpJsrOffset {
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub struct OpJsrRegister {
    pub jump_to_contents_of_register: Register,
}

impl From<u16> for OpJsrRegister {
    fn from(binary_instruction: u16) -> Self {
        let base_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();

        OpJsrRegister {
            jump_to_contents_of_register: base_register,
        }
    }
}

pub struct OpNot {
    pub destination_register: Register,
    pub first_value_register: Register,
}

impl From<u16> for OpNot {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let first_value_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();

        OpNot {
            destination_register: destination_register,
            first_value_register: first_value_register,
        }
    }
}

pub struct OpJmp {
    pub jump_to_contents_of_register: Register,
}

impl From<u16> for OpJmp {
    fn from(binary_instruction: u16) -> Self {
        let base_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();

        OpJmp {
            jump_to_contents_of_register: base_register,
        }
    }
}

pub struct OpLdr {
    pub destination_register: Register,
    pub base_registry: Register,
    pub offset: MemoryOffset,
}

impl From<u16> for OpLdr {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let base_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let offset = sign_extend(binary_instruction & 0b00111111, 6);

        OpLdr {
            destination_register: destination_register,
            base_registry: base_register,
            offset: convert_to_signed(offset),
        }
    }
}

pub struct OpLea {
    pub destination_register: Register,
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpLea {
    fn from(binary_instruction: u16) -> Self {
        let destination_register = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let program_counter_offset = sign_extend(binary_instruction & 0b000111111111, 9);

        OpLea {
            destination_register: destination_register,
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub struct OpSt {
    pub registry_to_store: Register,
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpSt {
    fn from(binary_instruction: u16) -> Self {
        let registry_to_store = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let program_counter_offset = sign_extend(binary_instruction & 0b000111111111, 9);

        OpSt {
            registry_to_store: registry_to_store,
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub struct OpStr {
    pub registry_to_store: Register,
    pub base_registry: Register,
    pub offset: MemoryOffset,
}

impl From<u16> for OpStr {
    fn from(binary_instruction: u16) -> Self {
        let registry_to_store = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let base_register = Register::from_u16((binary_instruction >> 6) & 0b0111).unwrap();
        let offset = sign_extend(binary_instruction & 0b00111111, 6);

        OpStr {
            registry_to_store: registry_to_store,
            base_registry: base_register,
            offset: convert_to_signed(offset),
        }
    }
}

pub struct OpSti {
    pub registry_to_store: Register,
    pub program_counter_offset: ProgramCounterOffset,
}

impl From<u16> for OpSti {
    fn from(binary_instruction: u16) -> Self {
        let registry_to_store = Register::from_u16((binary_instruction >> 9) & 0b0111).unwrap();
        let program_counter_offset = sign_extend(binary_instruction & 0b000111111111, 9);

        OpSti {
            registry_to_store: registry_to_store,
            program_counter_offset: convert_to_signed(program_counter_offset),
        }
    }
}

pub fn convert_to_signed(value: u16) -> i16 {
    unsafe {
        std::mem::transmute::<u16, i16>(value)
    }
}

pub fn convert_to_unsigned(value: i16) -> u16 {
    unsafe {
        std::mem::transmute::<i16, u16>(value)
    }
}

fn sign_extend(x: u16, bit_count: u16) -> u16 {
    let sign_bit = (x >> (bit_count - 1)) & 1;

    if sign_bit != 0 {
        let result = x | (0xFFFF << bit_count);
        return result;
    }

    return x;
}
