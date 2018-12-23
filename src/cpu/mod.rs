pub mod instructions;
mod registers;

use std::io::Read;
use self::instructions::Instruction;
use self::instructions::Address;
use self::instructions::TrapCode;
use self::instructions::ConditionFlag;

pub struct Cpu {
    memory: Vec<u16>,
    registers: Vec<u16>,
}

impl Cpu {
    pub fn new(program: Vec<u16>) -> Cpu {
        let mut cpu = Cpu {
            memory: program,
            registers: vec![0; registers::Register::RCount as usize],
        };

        cpu.registers[registers::Register::RPc as usize] = 12288;
        cpu
    }

    pub fn run_program(mut self) {
        loop {
            let instruction = self.next_instruction();
            self = self.process_instruction(instruction);
        }
    }

    fn next_instruction(&mut self) -> Instruction {
        let binary_instruction = self.next_binary_instruction();
        Instruction::from(binary_instruction)
    }

    fn next_binary_instruction(&mut self) -> instructions::BinaryInstruction {
        let program_counter = self.next_program_counter();
        let next_instruction = self.memory[program_counter as usize];
        next_instruction
    }

    fn next_program_counter(&mut self) -> u16 {
        let program_counter = self.registers[registers::Register::RPc as usize];
        self.registers[registers::Register::RPc as usize] = program_counter + 1;
        program_counter
    }

    fn process_instruction(self, instruction: Instruction) -> Self {
        match instruction {
            Instruction::OpAdd(instruction) => self.add(instruction),
            Instruction::OpAnd(instruction) => self.and(instruction),
            Instruction::OpNot(instruction) => self.not(instruction),
            Instruction::OpLdi(instruction) => self.load_indirect(instruction),
            Instruction::OpBr(instruction) => self.branch(instruction),
            Instruction::OpJmp(instruction) => self.jump(instruction),
            Instruction::OpJsr(instruction) => self.jump_register(instruction),
            Instruction::OpLoad(instruction) => self.load(instruction),
            Instruction::OpSt(instruction) => self.store(instruction),
            Instruction::OpSti(instruction) => self.store_indirect(instruction),
            Instruction::OpStr(instruction) => self.store_register(instruction),
            Instruction::OpLdr(instruction) => self.load_register(instruction),
            Instruction::OpLea(instruction) => self.load_effective_address(instruction),
            Instruction::OpTrap(trap_code) => self.trap(trap_code),
            Instruction::OpRti() => panic!("Not implemented"),
            Instruction::OpRes() => panic!("Not implemented"),
        }
    }

    fn read_memory(&self, memory_address: Address) -> u16 {
        self.memory[memory_address as usize]
    }
    
    fn set_memory(&mut self, memory_address: Address, value: u16) {
        self.memory[memory_address as usize] = value 
    }

    fn branch(mut self, instruction: instructions::OpBr) -> Self {
        let program_counter_offset = instruction.program_counter_offset;
        let condition_bits = self.registers[registers::Register::RCond as usize];
        let condition_register = instructions::ConditionFlag::from_bits_truncate(condition_bits);
        let should_branch = condition_register.intersects(instruction.condition_flag);

        if should_branch {
            let program_counter = self.registers[registers::Register::RPc as usize];
            let new_program_counter = program_counter as i16 + program_counter_offset;
            self.registers[registers::Register::RPc as usize] = new_program_counter as u16;
        }

        self
    }

    fn add(self, instruction: instructions::OpAdd) -> Self {
        match instruction {
            instructions::OpAdd::NormalMode(instruction) => self.add_normal(instruction),
            instructions::OpAdd::ImmediateMode(instruction) => self.add_immediate(instruction),
        }
    }

    fn add_normal(mut self, instruction: instructions::OpAddNormal) -> Self {
        let first_value_bits = self.registers[instruction.first_value_register as usize];
        let first_value = instructions::convert_to_signed(first_value_bits);
        let second_value = instructions::convert_to_signed(self.registers[instruction.second_value_register as usize]);
        let value = first_value.wrapping_add(second_value);
        let unsigned_value = instructions::convert_to_unsigned(value);

        self.registers[instruction.destination_register as usize] = unsigned_value;
        self.set_condition_flag(unsigned_value)
    }

    fn add_immediate(mut self, instruction: instructions::OpAddImmediate) -> Self {
        let first_value = instructions::convert_to_signed(self.registers[instruction.first_value_register as usize]);
        let value = first_value.wrapping_add(instruction.second_value);
        let unsigned_value = instructions::convert_to_unsigned(value);

        self.registers[instruction.destination_register as usize] = unsigned_value;
        self.set_condition_flag(unsigned_value)
    }

    fn load(mut self, instruction: instructions::OpLoad) -> Self {
        let destination_register = instruction.destination_register as usize;
        let program_counter = self.registers[registers::Register::RPc as usize];
        let program_counter_offset = instruction.program_counter_offset;
        let memory_address = program_counter as i16 + program_counter_offset;
        let memory_address_contents = self.read_memory(memory_address as u16);

        self.registers[destination_register as usize] = memory_address_contents;
        self.set_condition_flag(memory_address_contents)
    }

    fn store(mut self, instruction: instructions::OpSt) -> Self {
        let registry_contents = self.registers[instruction.registry_to_store as usize];
        let program_counter = self.registers[registers::Register::RPc as usize];
        let program_counter_offset = instruction.program_counter_offset;
        let memory_location = program_counter as i16 + program_counter_offset;

        self.set_memory(memory_location as u16, registry_contents);
        self
    }

    fn jump_register(self, instruction: instructions::OpJsr) -> Self {
        match instruction {
            instructions::OpJsr::Offset(instruction) => self.jump_register_offset(instruction),
            instructions::OpJsr::Register(instruction) => self.jump_register_register(instruction),
        }
    }

    fn jump_register_offset(mut self, instruction: instructions::OpJsrOffset) -> Self {
        let program_counter = self.registers[registers::Register::RPc as usize];
        self.registers[registers::Register::R7 as usize] = program_counter;

        let program_counter_offset = instruction.program_counter_offset;
        let new_program_counter = program_counter as i16 + program_counter_offset;

        self.registers[registers::Register::RPc as usize] = new_program_counter as u16;
        self
    }

    fn jump_register_register(mut self, instruction: instructions::OpJsrRegister) -> Self {
        let program_counter = self.registers[registers::Register::RPc as usize];
        self.registers[registers::Register::R7 as usize] = program_counter;

        let jump_register = instruction.jump_to_contents_of_register as usize;
        let jump_register_contents = self.registers[jump_register];

        self.registers[registers::Register::RPc as usize] = jump_register_contents;
        self
    }

    fn and(self, instruction: instructions::OpAnd) -> Self {
        match instruction {
            instructions::OpAnd::NormalMode(instruction) => self.and_normal(instruction),
            instructions::OpAnd::ImmediateMode(instruction) => self.and_immediate(instruction),
        }
    }

    fn and_normal(mut self, instruction: instructions::OpAndNormal) -> Self {
        let first_value = self.registers[instruction.first_value_register as usize];
        let second_value = self.registers[instruction.second_value_register as usize];

        let result = first_value & second_value;

        self.registers[instruction.destination_register as usize] = result;
        self.set_condition_flag(result)
    }

    fn and_immediate(mut self, instruction: instructions::OpAndImmediate) -> Self {
        let first_value = self.registers[instruction.first_value_register as usize];
        let second_value = instruction.second_value;

        let result = first_value & second_value;

        self.registers[instruction.destination_register as usize] = result;
        self.set_condition_flag(result)
    }

    fn load_register(mut self, instruction: instructions::OpLdr) -> Self {
        let destination_register = instruction.destination_register as usize;
        let base_registry_content = self.registers[instruction.base_registry as usize];
        let memory_address = instructions::convert_to_signed(base_registry_content) + instruction.offset;

        let value = self.read_memory(memory_address as u16);

        self.registers[destination_register] = value;
        self.set_condition_flag(value)
    }

    fn store_register(mut self, instruction: instructions::OpStr) -> Self {
        let registry_contents = self.registers[instruction.registry_to_store as usize];
        let base_registry_contents = instructions::convert_to_signed(self.registers[instruction.base_registry as usize]);
        let memory_address = base_registry_contents + instruction.offset;

        self.set_memory(memory_address as u16, registry_contents);
        self
    }

    fn not(mut self, instruction: instructions::OpNot) -> Self {
        let first_value = self.registers[instruction.first_value_register as usize];
        let result = !first_value;
        self.registers[instruction.destination_register as usize] = result;
        self.set_condition_flag(result)
    }

    fn load_indirect(mut self, instruction: instructions::OpLdi) -> Self {
        let program_counter = self.registers[registers::Register::RPc as usize] as i16;
        let offset = instruction.program_counter_offset;

        let memory_location = program_counter + offset;
        let final_memory_location = self.read_memory(memory_location as u16) as u16;
        let final_value = self.read_memory(final_memory_location);
        self.registers[instruction.destination_register as usize] = final_value;
        self.set_condition_flag(final_value)
    }

    fn store_indirect(mut self, instruction: instructions::OpSti) -> Self {
        let registry_contents = self.registers[instruction.registry_to_store as usize];
        let program_counter = self.registers[registers::Register::RPc as usize] as i16;
        let program_counter_offset = instruction.program_counter_offset;
        let memory_location = self.read_memory((program_counter + program_counter_offset) as u16) as u16;

        self.set_memory(memory_location, registry_contents);
        self
    }

    fn jump(mut self, instruction: instructions::OpJmp) -> Self {
        let register = instruction.jump_to_contents_of_register as usize;
        let register_value = self.registers[register];

        self.registers[registers::Register::RPc as usize] = register_value;
        self
    }

    fn load_effective_address(mut self, instruction: instructions::OpLea) -> Self {
        let destination_registry = instruction.destination_register as usize;
        let program_counter_offset = instruction.program_counter_offset;
        let program_counter = self.registers[registers::Register::RPc as usize];
        let new_program_counter = program_counter as i16 + program_counter_offset;

        self.registers[destination_registry] = new_program_counter as u16;
        self.set_condition_flag(instructions::convert_to_unsigned(new_program_counter))
    }

    fn trap(self, instruction: instructions::TrapCode) -> Self {
        match instruction {
            TrapCode::GetChar => self.trap_get_char(),
            TrapCode::OutChar => self.trap_out_char(),
            TrapCode::Puts => self.trap_puts(),
            TrapCode::Input => self.trap_input(),
            TrapCode::PutByteString => panic!(),
            TrapCode::Halt => panic!(),
        }
    }

    fn trap_puts(self) -> Self {
        let memory_address = self.registers[registers::Register::R0 as usize];
        let string_to_print = self.extract_string_from_memory_location(memory_address);
        
        print!("{}", string_to_print);
        self
    }

    fn extract_string_from_memory_location(&self, memory_location: Address) -> String {
        let array_slice = &self.memory[memory_location as usize..];

        array_slice
            .into_iter()
            .take_while(|ch| **ch != 0)
            .map(|ch| *ch as u8)
            .map(|ch| ch as char)
            .collect()
    }

    fn trap_get_char(mut self) -> Self {
        let character = std::io::stdin().bytes().next().unwrap().unwrap();
        self.registers[registers::Register::R0 as usize] = character.into();
        self
    }

    fn trap_input(mut self) -> Self {
        println!("Enter a character: ");
        let character = std::io::stdin().bytes().next().unwrap().unwrap();
        self.registers[registers::Register::R0 as usize] = character.into();
        self
    }

    fn trap_out_char(self) -> Self {
        let character = (self.registers[registers::Register::R0 as usize] as u8) as char;
        print!("{}", character);
        self
    }

    fn set_condition_flag(mut self, value: u16) -> Self {
        let condition_flag = calculate_condition_flag(value);
        self.registers[registers::Register::RCond as usize] = condition_flag.bits();
        self
    }
}

fn calculate_condition_flag(value: u16) -> ConditionFlag {
    if value == 0 {
        return ConditionFlag::Zero;
    }

    let has_negative_sign = (value >> 15) == 1;

    match has_negative_sign {
        true => ConditionFlag::Negative,
        false => ConditionFlag::Positive,
    }
}
