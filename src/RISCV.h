#pragma once
#include <string>
#include <stdint.h>
#include <memory>

class Memory;

class RISCV {
	std::unique_ptr<Memory> mem;
	uint32_t mem_start_addr = 0x0;

	uint32_t regs[32] = {};
	uint32_t pc = 0x0;

	uint32_t csrs[4096] = {};

	bool is_running = false;

	void execute_instruction();
	void execute_OP_IMM(uint32_t instr);
	void execute_LUI(uint32_t instr);
	void execute_AUIPC(uint32_t instr);
	void execute_OP(uint32_t instr);
	void execute_JAL(uint32_t instr);
	void execute_JALR(uint32_t instr);
	void execute_BRANCH(uint32_t instr);
	void execute_LOAD(uint32_t instr);
	void execute_STORE(uint32_t instr);
	void execute_MISC_MEM(uint32_t instr);
	void execute_SYSTEM(uint32_t instr);
	void print_context();

public:
	RISCV(std::unique_ptr<Memory> mem);
	void run(uint32_t entrypoint);
};

