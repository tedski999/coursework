library ieee;
use ieee.std_logic_1164.all;

entity processor_32bit is
	port(clock, reset : in std_logic);
end processor_32bit;

architecture Behavioral of processor_32bit is

	component register_file_32bit
		port(
			clock, load_enable           : in std_logic;
			td_dr, ta_sa, tb_sb          : in std_logic_vector(5 downto 0);
			input_data                   : in std_logic_vector(31 downto 0);
			a_output_data, b_output_data : out std_logic_vector(31 downto 0));
	end component;

	component functional_unit_32bit
		port(
			function_select  : in std_logic_vector(4 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			vcnz             : out std_logic_vector(3 downto 0));
	end component;

	component memory32_32bit
		port(
			clock, write_enable : in std_logic;
			input_data, address : in std_logic_vector(31 downto 0);
			output              : out std_logic_vector(31 downto 0));
	end component;

	component reg_32bit
		port(
			clock, load : in std_logic;
			input       : in std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	component program_counter_32bit
		port(
			clock, reset     : in std_logic;
			load_enable      : in std_logic;
			increment_enable : in std_logic;
			input            : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0));
	end component;

	component control_memory42_17bit
		port(
			input_addr     : in std_logic_vector(16 downto 0);
			mc, il, pi, pl : out std_logic;
			td, ta, tb, mb : out std_logic;
			md, rw, mm, mw : out std_logic;
			rv, rc, rn, rz : out std_logic;
			fl             : out std_logic;
			fs             : out std_logic_vector(4 downto 0);
			ms             : out std_logic_vector(2 downto 0);
			na             : out std_logic_vector(16 downto 0));
	end component;

	component control_address_register_17bit
		port(
			clock, reset      : in std_logic;
			increment_disable : in std_logic;
			input_addr        : in std_logic_vector(16 downto 0);
			output            : out std_logic_vector(16 downto 0));
	end component;

	component vcnz_flags_register
		port(
			clock, load_enable : in std_logic;
			vcnz, resets       : in std_logic_vector(3 downto 0);
			flags              : out std_logic_vector(3 downto 0));
	end component;

	component zerofill_5to32
		port(
			input  : in std_logic_vector(4 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	component extend_10to32
		port(
			input  : in std_logic_vector(9 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	component mux2_32bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	component mux2_17bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic_vector(16 downto 0);
			output       : out std_logic_vector(16 downto 0));
	end component;

	component mux8_1bit
		port(
			line_select : in std_logic_vector(2 downto 0);
			lines       : in std_logic_vector(7 downto 0);
			output      : out std_logic);
	end component;

	-- Signals between components
	signal bus_d, bus_a, bus_b, bus_m     : std_logic_vector(31 downto 0);
	signal instruction_addr, memory_addr  : std_logic_vector(31 downto 0);
	signal regfile_b_out, funcunit_out    : std_logic_vector(31 downto 0);
	signal immediate_operand, pc_input    : std_logic_vector(31 downto 0);
	signal opcode, car_in, car_out        : std_logic_vector(16 downto 0);
	signal dr, sa, sb                     : std_logic_vector(4 downto 0);
	signal vcnz, flags, inverted_flags    : std_logic_vector(3 downto 0);
	signal status_bit                     : std_logic;

	-- Control signals
	signal mc, il, pi, pl : std_logic;
	signal td, ta, tb, mb : std_logic;
	signal md, rw, mm, mw : std_logic;
	signal rv, rc, rn, rz : std_logic;
	signal fl             : std_logic;
	signal ms             : std_logic_vector(2 downto 0);
	signal fs             : std_logic_vector(4 downto 0);
	signal na             : std_logic_vector(16 downto 0);

begin

	inverted_flags <= not flags after 5 ns;

	-- The (32+1)x32-bit register file
	register_file: register_file_32bit port map(
		clock => clock, load_enable => rw,
		td_dr(5) => td, td_dr(4 downto 0) => dr,
		ta_sa(5) => ta, ta_sa(4 downto 0) => sa,
		tb_sb(5) => tb, tb_sb(4 downto 0) => sb,
		input_data => bus_d,
		a_output_data => bus_a, b_output_data => regfile_b_out);

	-- The 32-bit functional unit, with ALU and Shifter
	functional_unit: functional_unit_32bit port map(
		function_select => fs,
		a_input => bus_a, b_input => bus_b,
		output => funcunit_out, vcnz => vcnz);

	-- 32-bits of addressable 32-bit chunk memory
	memory: memory32_32bit port map(
		clock => clock, write_enable => mw,
		input_data => bus_b, address => memory_addr,
		output => bus_m);

	-- The 32-bit instruction register with 17-bit opcodes
	ir: reg_32bit port map(
		clock => clock, load => il, input => bus_m,
		output(31 downto 15) => opcode,
		output(14 downto 10) => dr,
		output(9 downto 5) => sa,
		output(4 downto 0) => sb);

	-- The 32-bit program counter
	pc: program_counter_32bit port map(
		clock => clock, reset => reset,
		load_enable => pl,
		increment_enable => pi,
		input => pc_input, output => instruction_addr);

	-- Control memory with 17-bit opcodes and 42-bit output
	control_memory: control_memory42_17bit port map(
		input_addr => car_out,
		mc => mc, il => il, pi => pi, pl => pl, td => td, ta => ta, tb => tb, mb => mb,
		md => md, rw => rw, mm => mm, mw => mw, rv => rv, rc => rc, rn => rn, rz => rz,
		fl => fl, fs => fs, ms => ms, na => na);

	-- The 17-bit control address register
	car: control_address_register_17bit port map(
		clock => clock, reset => reset,
		increment_disable => status_bit,
		input_addr => car_in, output => car_out);

	-- Status flags register
	flags_register: vcnz_flags_register port map(
		clock => clock, load_enable => fl, vcnz => vcnz,
		resets(0) => rv, resets(1) => rc,
		resets(2) => rn, resets(3) => rz,
		flags => flags);

	-- Convert sb to immediate_operand through zero filling
	immediate_operand_zerofill: zerofill_5to32 port map(
		input => sb,
		output => immediate_operand);

	-- Convert dr and sb to program counter input through extending
	extender: extend_10to32 port map(
		input(9 downto 5) => dr,
		input(4 downto 0) => sb,
		output => pc_input);

	-- Datapath multiplexers
	mux_b: mux2_32bit port map(line_select => mb, line0 => regfile_b_out, line1 => immediate_operand, output => bus_b);
	mux_d: mux2_32bit port map(line_select => md, line0 => funcunit_out, line1 => bus_m, output => bus_d);
	mux_m: mux2_32bit port map(line_select => mm, line0 => bus_a, line1 => instruction_addr, output => memory_addr);

	-- Control multiplexers
	mux_c: mux2_17bit port map(line_select => mc, line0 => na, line1 => opcode, output => car_in);
	mux_s: mux8_1bit port map(
		line_select => ms,
		lines(1 downto 0) => "10", -- NOTE: tied inputs 0 and 1 are low and high
		lines(5 downto 2) => flags,
		lines(6) => inverted_flags(0),
		lines(7) => inverted_flags(2),
		output => status_bit);
	
end Behavioral;

