#include "RISCV.h"

#include <fstream>
#include <iostream>
#include <iomanip>
#include <stdexcept>

#include "memory.h"

#define XLEN 32
#define DEBUG_PRINT false
#define SINGLE_STEP false

#define DEBUG(x) do{ if(DEBUG_PRINT) { (x); if (SINGLE_STEP) std::cin.ignore(); } } while(0)

constexpr uint32_t LOAD = 0b00'000;
constexpr uint32_t MISC_MEM = 0b00'011;
constexpr uint32_t OP_IMM = 0b00'100;
constexpr uint32_t STORE = 0b01'000;
constexpr uint32_t OP = 0b01'100;
constexpr uint32_t AUIPC = 0b00'101;
constexpr uint32_t LUI = 0b01'101;
constexpr uint32_t BRANCH = 0b11'000;
constexpr uint32_t JALR = 0b11'001;
constexpr uint32_t JAL = 0b11'011;
constexpr uint32_t SYSTEM = 0b11'100;

constexpr uint32_t ADDI = 0b000;
constexpr uint32_t SLTI = 0b010;
constexpr uint32_t SLTIU = 0b011;
constexpr uint32_t XORI = 0b100;
constexpr uint32_t ORI = 0b110;
constexpr uint32_t ANDI = 0b111;

constexpr uint32_t MUL = 0b000;
constexpr uint32_t MULH = 0b001;
constexpr uint32_t MULHSU = 0b010;
constexpr uint32_t MULHU = 0b011;
constexpr uint32_t DIV = 0b100;
constexpr uint32_t DIVU = 0b101;
constexpr uint32_t REM = 0b110;
constexpr uint32_t REMU = 0b111;

constexpr uint32_t ADD_SUB = 0b000;
constexpr uint32_t SLL = 0b001;
constexpr uint32_t SLT = 0b010;
constexpr uint32_t SLTU = 0b011;
constexpr uint32_t XOR = 0b100;
constexpr uint32_t SRL_SRA = 0b101;
constexpr uint32_t OR = 0b110;
constexpr uint32_t AND = 0b111;

RISCV::RISCV(std::unique_ptr<Memory> mem): mem(std::move(mem))
{
}
void RISCV::execute_OP_IMM(uint32_t instr)
{
	// Integer Register-Immediate Instructions
	uint32_t rd = (instr >> 7) & 0x1F;
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	int32_t imm = ((int32_t)instr >> 20);
	uint32_t result;
	uint32_t shamt;
	uint32_t shtyp;
	switch (funct3)
	{
		case ADDI:
			DEBUG(std::cout << "ADDI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (int32_t)regs[rs1] + imm;
			break;
		case SLTI:
			DEBUG(std::cout << "SLTI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (int32_t)regs[rs1] < (int32_t)imm;
			break;
		case SLTIU:
			DEBUG(std::cout << "SLTIU x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (uint32_t)regs[rs1] < (uint32_t)imm;
			break;
		case XORI:
			DEBUG(std::cout << "XORI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = regs[rs1] ^ imm;
			break;
		case ORI:
			DEBUG(std::cout << "ORI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = regs[rs1] | imm;
			break;
		case ANDI:
			DEBUG(std::cout << "ANDI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = regs[rs1] & imm;
			break;
		case 0x1:
			// SLLI
			shamt = imm & 0x1F;
			shtyp = (imm >> 5) & 0x7F;
			if (shtyp != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			DEBUG(std::cout << "SLLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
			result = regs[rs1] << shamt;
			break;
		case 0x5:
			// SRLI / SRAI
			shamt = imm & 0x1F;
			shtyp = (imm >> 5) & 0x7F;
			if ((shtyp & 0x5F) != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (shtyp) {
				DEBUG(std::cout << "SRAI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
				result = (int32_t)regs[rs1] >> (int32_t)shamt;
			}
			else {
				DEBUG(std::cout << "SRLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
				result = regs[rs1] >> shamt;
			}
			break;
		default:
			// TODO: throw illegal instruction exception
			return;
	}
	if (rd != 0) regs[rd] = result;
	pc += 4;
}

void RISCV::execute_LUI(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	DEBUG(std::cout << "LUI x" << rd << ", " << (int32_t)(instr & 0xFFFFF000) << std::endl);
	if (rd != 0) regs[rd] = (int32_t)(instr & 0xFFFFF000);
	pc += 4;
}

void RISCV::execute_AUIPC(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	DEBUG(std::cout << "AUIPC x" << rd << ", " << (int32_t)(instr & 0xFFFFF000) << std::endl);
	if (rd != 0) regs[rd] = pc + (int32_t)(instr & 0xFFFFF000);
	pc += 4;
}

void RISCV::execute_OP(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t rs2 = (instr >> 20) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	uint32_t funct7 = (instr >> 25) & 0x7F;
	uint32_t result;
	if (funct7 & 0x1) {
		if ((funct7 & 0x7E) != 0x0) {
			// TODO: throw illegal instruction exception
			return;
		}
		switch (funct3)
		{
			case MUL:
				DEBUG(std::cout << "MUL x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)((int32_t)regs[rs1] * (int32_t)regs[rs2]);
				break;
			case MULH:
				DEBUG(std::cout << "MULH x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (int64_t)((int32_t)regs[rs2])) >> 32);
				break;
			case 0x3:
				DEBUG(std::cout << "MULHU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (uint32_t)(((uint64_t)((uint32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			case MULHSU:
				DEBUG(std::cout << "MULHSU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			case DIV:
				DEBUG(std::cout << "DIV x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				if (regs[rs2] == 0) { // Div by 0
					result = -1;
					break;
				}
				else if (regs[rs1] == 0x80000000 && regs[rs2] == 0xffffffff) { // Signed Div overflow
					result = regs[rs1];
					break;
				}
				result = (int32_t)((int32_t)regs[rs1] / (int32_t)regs[rs2]);
				break;
			case DIVU:
				DEBUG(std::cout << "DIVU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				if (regs[rs2] == 0) {
					result = ((uint64_t)1 << XLEN) - 1;
					break;
				}
				result = regs[rs1] / regs[rs2];
				break;
			case REM:
				DEBUG(std::cout << "REM x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				if (regs[rs2] == 0) { // Div by 0
					result = regs[rs1];
					break;
				}
				else if (regs[rs1] == 0x80000000 && regs[rs2] == 0xffffffff) { // Signed Div overflow
					result = 0;
					break;
				}
				result = (int32_t)((int32_t)regs[rs1] % (int32_t)regs[rs2]);
				break;
			case REMU:
				DEBUG(std::cout << "REMU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				if (regs[rs2] == 0) {
					result = regs[rs1];
					break;
				}
				result = regs[rs1] / regs[rs2];
				break;
			default:
				// TODO: throw illegal instruction exception
				return;
		}
	}
	else
	{
		if ((funct7 & 0x5F) != 0x0) {
			// TODO: throw illegal instruction exception
			return;
		}
		switch (funct3)
		{
			case ADD_SUB:
				if (funct7) {
					DEBUG(std::cout << "SUB x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)(regs[rs1] - regs[rs2]);
				}
				else {
					DEBUG(std::cout << "ADD x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)(regs[rs1] + regs[rs2]);
				}
				break;
			case SLT:
				DEBUG(std::cout << "SLT x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)regs[rs1] < (int32_t)regs[rs2];
				break;
			case SLTU:
				DEBUG(std::cout << "SLTU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (uint32_t)regs[rs1] < (uint32_t)regs[rs2];
				break;
			case XOR:
				DEBUG(std::cout << "XOR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] ^ regs[rs2];
				break;
			case OR:
				DEBUG(std::cout << "OR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] | regs[rs2];
				break;
			case AND:
				DEBUG(std::cout << "AND x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] & regs[rs2];
				break;
			case SLL:
				DEBUG(std::cout << "SLLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] << (regs[rs2] & 0x1F);
				break;
			case SRL_SRA:
				if (funct7) {
					DEBUG(std::cout << "SRAI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)regs[rs1] >> (int32_t)(regs[rs2] & 0x1F);
				}
				else {
					DEBUG(std::cout << "SRLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = regs[rs1] >> (regs[rs2] & 0x1F);
				}
				break;
			default:
				// TODO: throw illegal instruction exception
				return;
		}
	}
	if (rd != 0) regs[rd] = result;
	pc += 4;
}

void RISCV::execute_JAL(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	int32_t offset = (((instr >> 21) & 0x3FF) | (((instr >> 20) & 0x1) << 10) | (((instr >> 12) & 0xFF) << 11) | ((((int32_t)instr) >> 31)) << 19) << 1;
	DEBUG(std::cout << "JAL x" << rd << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
	if (rd != 0)
		regs[rd] = pc + 4;
	uint32_t addr = (int32_t)pc + offset;
	if (addr & 3) {
		// TODO: throw misaligned instruction exception
		return;
	}
	pc = addr;
}

void RISCV::execute_JALR(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	if (funct3 != 0x0) {
		// TODO: throw illegal instruction exception
		return;
	}
	int32_t offset = ((int32_t)instr) >> 20;
	DEBUG(std::cout << "JALR x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
	if (rd != 0)
		regs[rd] = pc+4;
	uint32_t addr = (((int32_t)regs[rs1] + offset) >> 1) << 1;
	if (addr & 3) {
		// TODO: throw misaligned instruction exception
		return;
	}
	pc = addr;
}

void RISCV::execute_BRANCH(uint32_t instr)
{
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t rs2 = (instr >> 20) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	int32_t offset = (((instr >> 8) & 0xF) | (((instr >> 25) & 0x3F) << 4) | (((instr >> 7) & 0x1) << 10) | ((((int32_t)instr) >> 31)) << 11) << 1;
	bool should_branch = false;
	switch (funct3)
	{
		case 0x0: { // BEQ
			DEBUG(std::cout << "BEQ x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] == regs[rs2];
			break;
		}
		case 0x1: { // BNE
			DEBUG(std::cout << "BNE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] != regs[rs2];
			break;
		}
		case 0x4: { // BLT
			DEBUG(std::cout << "BLT x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = (int32_t)regs[rs1] < (int32_t)regs[rs2];
			break;
		}
		case 0x6: { // BLTU
			DEBUG(std::cout << "BLTU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] < regs[rs2];
			break;
		}
		case 0x5: { // BGE
			DEBUG(std::cout << "BGE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = (int32_t)regs[rs1] >= (int32_t)regs[rs2];
			break;
		}
		case 0x7: { // BGEU
			DEBUG(std::cout << "BGEU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] >= regs[rs2];
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
	}
	if (should_branch) {
		uint32_t addr = (int32_t)pc + offset;
		if (addr & 3) {
			// TODO: throw misaligned instruction exception
			return;
		}
		pc += offset;
	}
	else {
		pc += 4;
	}
}

void RISCV::execute_LOAD(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	int32_t offset = ((int32_t)instr) >> 20;
	switch (funct3)
	{
		case 0x0: { // LB
			DEBUG(std::cout << "LB x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint8_t result;
			if (mem->read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int8_t)result);
			break;
		}
		case 0x4: { // LBU
			DEBUG(std::cout << "LBU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint8_t result;
			if (mem->read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x1: { // LH
			DEBUG(std::cout << "LH x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint16_t result;
			if (mem->read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int16_t)result);
			break;
		}
		case 0x5: { // LHU
			DEBUG(std::cout << "LHU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint16_t result;
			if (mem->read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x2: { // LW
			DEBUG(std::cout << "LW x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint32_t result;
			if (mem->read_u32(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = result;
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
	}
	pc += 4;
}

void RISCV::execute_STORE(uint32_t instr)
{
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t rs2 = (instr >> 20) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	int32_t offset = ((instr >> 7) & 0x1F) | ((((int32_t)instr) >> 25) << 5);
	switch (funct3) {
	case 0x0: { // SB
		DEBUG(std::cout << "SB x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
		if (mem->write_u8(regs[rs1] + offset, regs[rs2] & 0xFF)) {
			// TODO: handle raised exceptions
		}
		break;
	}
	case 0x1: { // SH
		DEBUG(std::cout << "SH x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
		if (mem->write_u16(regs[rs1] + offset, regs[rs2] & 0xFFFF)) {
			// TODO: handle raised exceptions
		}
		break;
	}
	case 0x2: { // SW
		DEBUG(std::cout << "SW x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
		if (mem->write_u32(regs[rs1] + offset, regs[rs2])) {
			// TODO: handle raised exceptions
		}
		break;
	}
	default: {
		// TODO: throw illegal instruction exception
		return;
	}
	}
	pc += 4;
}

void RISCV::execute_MISC_MEM(uint32_t instr)
{
	uint32_t funct3 = (instr >> 12) & 0x7;
	switch (funct3)
	{
		case 0x0: { // FENCE
			DEBUG(std::cout << "FENCE ???" << std::endl);
			// TODO: either implement fence or throw an illegal instruction exception
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
	}
	pc += 4;
}

void RISCV::execute_SYSTEM(uint32_t instr)
{
	uint32_t rd = (instr >> 7) & 0x1F;
	uint32_t rs1 = (instr >> 15) & 0x1F;
	uint32_t funct3 = (instr >> 12) & 0x7;
	if (funct3)
	{ // Zicsr Instructions
		uint32_t csr = (instr >> 20) & 0xFFF;
		switch (funct3) {
		case 0x1: { // CSRRW
			DEBUG(std::cout << "CSRRW x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
			if (rd != 0) {
				uint32_t old_value = csrs[csr];
				regs[rd] = old_value;
			}
			csrs[csr] = regs[rs1];
			break;
		}
		case 0x2: { // CSRRS
			DEBUG(std::cout << "CSRRS x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
			uint32_t old_value = csrs[csr];
			if (rd != 0) {
				regs[rd] = old_value;
			}
			if (rs1 != 0) {
				csrs[csr] = old_value | regs[rs1];
			}
			break;
		}
		case 0x3: { // CSRRC
			DEBUG(std::cout << "CSRRC x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
			uint32_t old_value = csrs[csr];
			if (rd != 0) {
				regs[rd] = old_value;
			}
			if (rs1 != 0) {
				csrs[csr] = old_value & (~regs[rs1]);
			}
			break;
		}
		case 0x5: { // CSRRWI
			DEBUG(std::cout << "CSRRWI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
			if (rd != 0) {
				uint32_t old_value = csrs[csr];
				regs[rd] = old_value;
			}
			csrs[csr] = rs1; // not actually a register number, a 5 bit zero-extended immediate
			break;
		}
		case 0x6: { // CSRRSI
			DEBUG(std::cout << "CSRRSI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
			uint32_t old_value = csrs[csr];
			if (rd != 0) {
				regs[rd] = old_value;
			}
			if (rs1 != 0) {
				csrs[csr] = old_value | rs1; // not actually a register number, a 5 bit zero-extended immediate
			}
			break;
		}
		case 0x7: { // CSRRCI
			DEBUG(std::cout << "CSRRCI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
			uint32_t old_value = csrs[csr];
			if (rd != 0) {
				regs[rd] = old_value;
			}
			if (rs1 != 0) {
				csrs[csr] = old_value & (~rs1); // not actually a register number, a 5 bit zero-extended immediate
			}
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
	}
	else
	{ // PRIV
		uint32_t funct12 = (instr >> 20) & 0xFFF;
		if (rd != 0 || rs1 != 0) {
			// TODO: throw illegal instruction exception
			return;
		}
		if (funct12 & 0xFFE) {
			// TODO: throw illegal instruction exception
			return;
		}
		if (funct12) { // EBREAK
			DEBUG(std::cout << "EBREAK" << std::endl);
			// TODO: throw breakpoint exception
			return;
		}
		else { // ECALL
			DEBUG(std::cout << "ECALL" << std::endl);
			// TODO: implement various services
			if (regs[3] & 1) { // system exit
				std::cout << "program exited with code " << (regs[3] >> 1) << std::endl;
				is_running = false;
			}
			return;
		}
	}
	pc += 4;
}

void RISCV::execute_instruction()
{
	uint32_t insn;
	if (mem->read_u32(pc, &insn))
			throw std::runtime_error("Failed fetching next instruction!");

	if (insn == 0) { // TODO: remove this
			is_running = false;
			return;
		}

	uint32_t opcode = insn & 0x7F;
	if (opcode & 0x3 != 0x3) {
		// TODO: throw illegal instruction exception
		return;
	}
	opcode >>= 2;
	uint32_t rd = (insn >> 7) & 0x1F;
	uint32_t rs1 = (insn >> 15) & 0x1F;
	uint32_t rs2 = (insn >> 20) & 0x1F;
	switch (opcode)
	{
		case OP_IMM:
			execute_OP_IMM(insn);
			break;
		case LUI:
			execute_LUI(insn);
			break;
		case AUIPC:
			execute_AUIPC(insn);
			break;
		case OP:
			execute_OP(insn);
			break;
		case JAL:
			execute_JAL(insn);
			break;
		case JALR:
			execute_JALR(insn);
			break;
		case BRANCH:
			execute_BRANCH(insn);
			break;
		case LOAD:
			execute_LOAD(insn);
			break;
		case STORE:
			execute_STORE(insn);
			break;
		case MISC_MEM:
			execute_MISC_MEM(insn);
			break;
		case SYSTEM:
			execute_SYSTEM(insn);
			break;
		default:
			// TODO: throw illegal instruction exception
			return;
	}
}

void RISCV::run(uint32_t entrypoint) {
	pc = entrypoint;
	is_running = true;
	regs[0] = 0; // should be a noop
	regs[2] = mem_start_addr + MEM_SIZE - 4; // set stack pointer incase program assumes it exists
	while (is_running)
	{
		if (DEBUG_PRINT) print_context();
		execute_instruction();
	}
}

void RISCV::print_context() {
	for (int i = 0; i < 32; i++)
	{
		std::cout << "| x" << i << ": " << regs[i] << "(" << (int32_t)regs[i] << ") ";
	}
	std::cout << "| pc: 0x" << std::setbase(16) << pc << std::setbase(10) << std::endl;
}
