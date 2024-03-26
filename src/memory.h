#pragma once

#include <stdint.h>

#define MEM_SIZE 0x10000

class Memory
{
    public:
        Memory(const char* program_filename, uint32_t mem_start, uint32_t size);
        int read_u8(uint32_t addr, uint8_t* out);
        int read_u16(uint32_t addr, uint16_t* out);
        int read_u32(uint32_t addr, uint32_t* out);
        int write_u8(uint32_t addr, uint8_t in);
        int write_u16(uint32_t addr, uint16_t in);
        int write_u32(uint32_t addr, uint32_t in);

    private:
        uint32_t mem_start_addr = 0x0;
        uint8_t mem[MEM_SIZE] = {};
};
