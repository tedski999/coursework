library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity control_memory42_17bit is
	port (
		input_addr     : in std_logic_vector(16 downto 0);
		mc, il, pi, pl : out std_logic;                      -- ports 21-18
		td, ta, tb, mb : out std_logic;                      -- ports 17-14
		md, rw, mm, mw : out std_logic;                      -- ports 8-5
		rv, rc, rn, rz : out std_logic;                      -- ports 4-1
		fl             : out std_logic;                      -- port 0
		fs             : out std_logic_vector(4 downto 0);   -- ports 13 downto 9
		ms             : out std_logic_vector(2 downto 0);   -- ports 24 downto 22
		na             : out std_logic_vector(16 downto 0)); -- ports 41 downto 25
end control_memory42_17bit;

architecture Behavioral of control_memory42_17bit is

	type t_memory_array is array(0 to 255) of std_logic_vector(41 downto 0);
	signal control_memory_array : t_memory_array := (

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr | label | comment
		"000000000000000000010000000011111000000000",  -- 0x00 | HCF   | Halts the program execution - Typically only executed when in an erroneous state
		"000000000000000100010110000000000001000000",  -- 0x01 | ...   | Load instruction from address in PC into IR, increment PC
		"000000000000000000011000000000000000000000",  -- 0x02 | ...   | Executes the instruction in IR on the next clock cycle (moves the opcode into CAR)
		"000000000000000010010000000000000000000000",  -- 0x03 | NOP   | Takes a well deserved break
		"000000000000000010010000000000000010000000",  -- 0x04 | MOV   | Copies the contents of reg A into DST reg
		"000000000000000010010000000000000110000000",  -- 0x05 | LD    | Load value from the address pointed to by reg A into DST reg
		"000000000000000010010000000000000000100000",  -- 0x06 | ST    | Writes value in reg B to address pointed to by reg A
		"000000000000000010010000000110000010000000",  -- 0x07 | LDI   | Loads the immediate value into DST reg
		"000000000000000010010000000100000000100000",  -- 0x08 | STI   | Writes the immediate value into the address pointed to by reg A
		"000000000000000010010000000000010010000000",  -- 0x09 | ADD   | Writes the addition of reg A and reg B into DST reg
		"000000000000000010010000000100010010000000",  -- 0x0a | ADI   | Writes the addition of reg A and the immediate value into DST reg
		"000000000000000010010000000000101010000000",  -- 0x0b | SUB   | Writes the subtraction of reg A and reg B into DST reg
		"000000000000000010010000000100000010000000",  -- 0x0c | SBI   | Writes the subtraction of reg A and the immediate value into DST reg
		"000000000000000010010000000000001010000000",  -- 0x0d | INC   | Increments reg A and writes the result to DST reg
		"000000000000000010010000000000110010000000",  -- 0x0e | DEC   | Decrements reg A and writes the result to DST reg
		"000000000000000010010000000001000010000000",  -- 0x0f | AND   | Writes the logical AND of reg A and reg B into DST reg
		"000000000000000010010000000001010010000000",  -- 0x10 | OR    | Writes the logical OR of reg A and reg B into DST reg
		"000000000000000010010000000001100010000000",  -- 0x11 | XOR   | Writes the logical XOR of reg A and reg B into DST reg
		"000000000000000010010000000001110010000000",  -- 0x12 | NOT   | Writes the logical NOT of reg A into DST reg
		"000000000000000010010000000010100010000000",  -- 0x13 | SR    | Bitwise shifts reg B right and writes the result into DST reg
		"000000000000000010010000000011000010000000",  -- 0x14 | SL    | Bitwise shifts reg B left and writes the result into DST reg
		"000000000000000010010000000110100010000000",  -- 0x15 | SRI   | Bitwise shifts the immediate value right and writes the result into DST reg
		"000000000000000010010000000111000010000000",  -- 0x16 | SLI   | Bitwise shifts the immediate value left and writes the result into DST reg
		"000000000000000010010000000000101000000001",  -- 0x17 | CMP   | Compares reg A with reg B, sets flags appropriately
		"000000000000000010010000000100101000000001",  -- 0x18 | CPI   | Compares reg A with the immediate value, sets flags appropriately
		"000000000000111010100000000000000000000000",  -- 0x19 | JZ    | Jumps if Z flag is set - Could also be known as JEQ (prev. CMP was equal)
		"000000000000111011100000000000000000000000",  -- 0x1a | JNZ   | Jumps if Z flag is not set - Could also be known as JNE (prev. CMP was not equal)
		"000000000000111011000000000000000000000000",  -- 0x1b | JC    | Jumps if C flag is set - Could also be known as JGE (prev. CMP A was greater than or equal to B)
		"000000000000111011110000000000000000000000",  -- 0x1c | JNC   | Jumps if C flag is not set - Could also be known as JLT (prev. CMP A was less than B)
		"000000000000000010010001000000000000000000",  -- 0x1d | J     | Unconditional jump
		"000000000000000010010000000000000000000000",  -- 0x1e | ...   | NOP executed instead of J if a conditional jump fails
		"000000000000000000010000000000000000000000",  -- 0x1f | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0x20 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x21 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x22 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x23 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x24 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x25 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x26 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x27 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x28 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x29 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x2f | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x30 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x31 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x32 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x33 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x34 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x35 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x36 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x37 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x38 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x39 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x3f | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0x40 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x41 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x42 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x43 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x44 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x45 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x46 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x47 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x48 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x49 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x4f | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x50 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x51 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x52 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x53 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x54 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x55 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x56 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x57 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x58 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x59 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x5f | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0x60 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x61 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x62 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x63 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x64 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x65 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x66 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x67 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x68 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x69 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x6f | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x70 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x71 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x72 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x73 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x74 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x75 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x76 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x77 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x78 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x79 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x7f | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0x80 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x81 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x82 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x83 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x84 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x85 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x86 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x87 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x88 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x89 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x8f | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x90 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x91 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x92 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x93 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x94 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x95 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x96 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x97 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x98 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x99 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9a | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9b | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9c | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9d | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9e | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0x9f | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0xa0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xa9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xaa | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xab | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xac | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xad | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xae | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xaf | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xb9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xba | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xbb | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xbc | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xbd | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xbe | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xbf | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0xc0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xc9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xca | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xcb | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xcc | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xcd | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xce | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xcf | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xd9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xda | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xdb | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xdc | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xdd | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xde | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xdf | ERR   | Invalid opcode address

--		|N                M  MIPPTTTMF    MRMMRRRRF|   --
--		|A................S..CLILDABBS....DWMWVCNZL|   -- addr
		"000000000000000000010000000000000000000000",  -- 0xe0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xe9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xea | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xeb | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xec | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xed | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xee | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xef | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf0 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf1 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf2 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf3 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf4 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf5 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf6 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf7 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf8 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xf9 | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xfa | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xfb | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xfc | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xfd | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000",  -- 0xfe | ERR   | Invalid opcode address
		"000000000000000000010000000000000000000000"); -- 0xff | ERR   | Invalid opcode address

begin

	na <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(41 downto 25);
	ms <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(24 downto 22);
	mc <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(21);
	il <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(20);
	pi <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(19);
	pl <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(18);
	td <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(17);
	ta <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(16);
	tb <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(15);
	mb <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(14);
	fs <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(13 downto 9);
	md <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(8);
	rw <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(7);
	mm <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(6);
	mw <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(5);
	rv <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(4);
	rc <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(3);
	rn <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(2);
	rz <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(1);
	fl <= control_memory_array(to_integer(unsigned(input_addr(7 downto 0))))(0);

end Behavioral;

