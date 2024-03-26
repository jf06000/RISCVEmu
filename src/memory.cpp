#include "memory.h"

#include <iostream>
#include <fstream>

Memory::Memory(const char* program_filename, uint32_t mem_start, uint32_t size): mem_start_addr(mem_start)
{
    std::ifstream program_file(program_filename, std::ios::in | std::ios::binary);
	if (!program_file.is_open()) {
		throw std::runtime_error("Failed to open program file!");
	}
	program_file.read((char*)mem, MEM_SIZE);
	program_file.close();
}

int Memory::read_u8(uint32_t addr, uint8_t* out) {
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 1) {
		*out = 0;
		std::cerr << "Illegal mem read 8 addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr];
	}
	return 0;
}

int Memory::read_u16(uint32_t addr, uint16_t* out) {
	if (addr & 1) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 2) {
		*out = 0;
		std::cerr << "Illegal mem read 16 addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr] | (mem[addr + 1] << 8);
	}
	return 0;
}

int Memory::read_u32(uint32_t addr, uint32_t* out) {
	if (addr & 3) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 4) {
		*out = 0;
		std::cerr << "Illegal mem read 32 addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr] | (mem[addr + 1] << 8) | (mem[addr + 2] << 16) | (mem[addr + 3] << 24);
	}
	return 0;
}

int Memory::write_u8(uint32_t addr, uint8_t in) {
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 1) {
		std::cerr << "Illegal mem write 8 addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in;
	}
	return 0;
}

int Memory::write_u16(uint32_t addr, uint16_t in) {
	if (addr & 1) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 2) {
		std::cerr << "Illegal mem write 16 addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in & 0xFF;
		mem[addr + 1] = (in >> 8) & 0xFF;
	}
	return 0;
}

int Memory::write_u32(uint32_t addr, uint32_t in) {
	if (addr & 3) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 4) {
		std::cerr << "Illegal mem write 32 addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in & 0xFF;
		mem[addr + 1] = (in >> 8) & 0xFF;
		mem[addr + 2] = (in >> 16) & 0xFF;
		mem[addr + 3] = (in >> 24) & 0xFF;
	}
	return 0;
}
