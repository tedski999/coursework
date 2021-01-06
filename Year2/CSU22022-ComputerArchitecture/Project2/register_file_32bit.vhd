library ieee;
use ieee.std_logic_1164.all;

entity register_file_32bit is
	port(
		clock, load_enable           : in std_logic;
		td_dr, ta_sa, tb_sb          : in std_logic_vector(5 downto 0);
		input_data                   : in std_logic_vector(31 downto 0);
		a_output_data, b_output_data : out std_logic_vector(31 downto 0));
end register_file_32bit;

architecture Behavioral of register_file_32bit is

	component decoder_5to32
		port(
			input  : in std_logic_vector(4 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	component reg_32bit
		port(
			clock, load : in std_logic;
			input       : in std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	component mux32_32bit
		port(
			line_select                    : in std_logic_vector(4 downto 0);
			line00, line01, line02, line03 : in std_logic_vector(31 downto 0);
			line04, line05, line06, line07 : in std_logic_vector(31 downto 0);
			line08, line09, line0a, line0b : in std_logic_vector(31 downto 0);
			line0c, line0d, line0e, line0f : in std_logic_vector(31 downto 0);
			line10, line11, line12, line13 : in std_logic_vector(31 downto 0);
			line14, line15, line16, line17 : in std_logic_vector(31 downto 0);
			line18, line19, line1a, line1b : in std_logic_vector(31 downto 0);
			line1c, line1d, line1e, line1f : in std_logic_vector(31 downto 0);
			output                         : out std_logic_vector(31 downto 0));
	end component;

	signal temp_reg_load_enable                                   : std_logic;
	signal dst_select_vector, load_vector, a_data, b_data         : std_logic_vector(31 downto 0);
	signal reg00_output, reg01_output, reg02_output, reg03_output : std_logic_vector(31 downto 0);
	signal reg04_output, reg05_output, reg06_output, reg07_output : std_logic_vector(31 downto 0);
	signal reg08_output, reg09_output, reg0a_output, reg0b_output : std_logic_vector(31 downto 0);
	signal reg0c_output, reg0d_output, reg0e_output, reg0f_output : std_logic_vector(31 downto 0);
	signal reg10_output, reg11_output, reg12_output, reg13_output : std_logic_vector(31 downto 0);
	signal reg14_output, reg15_output, reg16_output, reg17_output : std_logic_vector(31 downto 0);
	signal reg18_output, reg19_output, reg1a_output, reg1b_output : std_logic_vector(31 downto 0);
	signal reg1c_output, reg1d_output, reg1e_output, reg1f_output : std_logic_vector(31 downto 0);
	signal reg20_output                                           : std_logic_vector(31 downto 0);

begin

	-- TODO: this is faaurrked - comment barely helps

	-- The dst, a and b register selection can be overridden by load_enable/td, ta and tb
	load_vector <= dst_select_vector when (load_enable = '1') and (td_dr(5) = '0') else x"00000000" after 5 ns;
	temp_reg_load_enable <= '1' when (load_enable = '1') and (td_dr(5) = '1') else '0' after 5 ns;
	a_output_data <= a_data when (ta_sa(5) = '0') else reg20_output after 5 ns;
	b_output_data <= b_data when (tb_sb(5) = '0') else reg20_output after 5 ns;

	-- Destination register decoder
	dst_decoder: decoder_5to32 port map(input => td_dr(4 downto 0), output => dst_select_vector);
	
	-- 32 32-bit registers
	reg00: reg_32bit port map(clock => clock, load => load_vector(0),  input => input_data, output => reg00_output);
	reg01: reg_32bit port map(clock => clock, load => load_vector(1),  input => input_data, output => reg01_output);
	reg02: reg_32bit port map(clock => clock, load => load_vector(2),  input => input_data, output => reg02_output);
	reg03: reg_32bit port map(clock => clock, load => load_vector(3),  input => input_data, output => reg03_output);
	reg04: reg_32bit port map(clock => clock, load => load_vector(4),  input => input_data, output => reg04_output);
	reg05: reg_32bit port map(clock => clock, load => load_vector(5),  input => input_data, output => reg05_output);
	reg06: reg_32bit port map(clock => clock, load => load_vector(6),  input => input_data, output => reg06_output);
	reg07: reg_32bit port map(clock => clock, load => load_vector(7),  input => input_data, output => reg07_output);
	reg08: reg_32bit port map(clock => clock, load => load_vector(8),  input => input_data, output => reg08_output);
	reg09: reg_32bit port map(clock => clock, load => load_vector(9),  input => input_data, output => reg09_output);
	reg0a: reg_32bit port map(clock => clock, load => load_vector(10), input => input_data, output => reg0a_output);
	reg0b: reg_32bit port map(clock => clock, load => load_vector(11), input => input_data, output => reg0b_output);
	reg0c: reg_32bit port map(clock => clock, load => load_vector(12), input => input_data, output => reg0c_output);
	reg0d: reg_32bit port map(clock => clock, load => load_vector(13), input => input_data, output => reg0d_output);
	reg0e: reg_32bit port map(clock => clock, load => load_vector(14), input => input_data, output => reg0e_output);
	reg0f: reg_32bit port map(clock => clock, load => load_vector(15), input => input_data, output => reg0f_output);
	reg10: reg_32bit port map(clock => clock, load => load_vector(16), input => input_data, output => reg10_output);
	reg11: reg_32bit port map(clock => clock, load => load_vector(17), input => input_data, output => reg11_output);
	reg12: reg_32bit port map(clock => clock, load => load_vector(18), input => input_data, output => reg12_output);
	reg13: reg_32bit port map(clock => clock, load => load_vector(19), input => input_data, output => reg13_output);
	reg14: reg_32bit port map(clock => clock, load => load_vector(20), input => input_data, output => reg14_output);
	reg15: reg_32bit port map(clock => clock, load => load_vector(21), input => input_data, output => reg15_output);
	reg16: reg_32bit port map(clock => clock, load => load_vector(22), input => input_data, output => reg16_output);
	reg17: reg_32bit port map(clock => clock, load => load_vector(23), input => input_data, output => reg17_output);
	reg18: reg_32bit port map(clock => clock, load => load_vector(24), input => input_data, output => reg18_output);
	reg19: reg_32bit port map(clock => clock, load => load_vector(25), input => input_data, output => reg19_output);
	reg1a: reg_32bit port map(clock => clock, load => load_vector(26), input => input_data, output => reg1a_output);
	reg1b: reg_32bit port map(clock => clock, load => load_vector(27), input => input_data, output => reg1b_output);
	reg1c: reg_32bit port map(clock => clock, load => load_vector(28), input => input_data, output => reg1c_output);
	reg1d: reg_32bit port map(clock => clock, load => load_vector(29), input => input_data, output => reg1d_output);
	reg1e: reg_32bit port map(clock => clock, load => load_vector(30), input => input_data, output => reg1e_output);
	reg1f: reg_32bit port map(clock => clock, load => load_vector(31), input => input_data, output => reg1f_output);

	-- 1 32-bit temp register
	reg20: reg_32bit port map(clock => clock, load => temp_reg_load_enable, input => input_data, output => reg20_output);

	-- Bus A register select multiplexer
	a_select_mux: mux32_32bit port map(
		line_select => ta_sa(4 downto 0),
		line00 => reg00_output, line01 => reg01_output, line02 => reg02_output, line03 => reg03_output,
		line04 => reg04_output, line05 => reg05_output, line06 => reg06_output, line07 => reg07_output,
		line08 => reg08_output, line09 => reg09_output, line0a => reg0a_output, line0b => reg0b_output,
		line0c => reg0c_output, line0d => reg0d_output, line0e => reg0e_output, line0f => reg0f_output,
		line10 => reg10_output, line11 => reg11_output, line12 => reg12_output, line13 => reg13_output,
		line14 => reg14_output, line15 => reg15_output, line16 => reg16_output, line17 => reg17_output,
		line18 => reg18_output, line19 => reg19_output, line1a => reg1a_output, line1b => reg1b_output,
		line1c => reg1c_output, line1d => reg1d_output, line1e => reg1e_output, line1f => reg1f_output,
		output => a_data);

	-- Bus B register select multiplexer
	b_select_mux: mux32_32bit port map(
		line_select => tb_sb(4 downto 0),
		line00 => reg00_output, line01 => reg01_output, line02 => reg02_output, line03 => reg03_output,
		line04 => reg04_output, line05 => reg05_output, line06 => reg06_output, line07 => reg07_output,
		line08 => reg08_output, line09 => reg09_output, line0a => reg0a_output, line0b => reg0b_output,
		line0c => reg0c_output, line0d => reg0d_output, line0e => reg0e_output, line0f => reg0f_output,
		line10 => reg10_output, line11 => reg11_output, line12 => reg12_output, line13 => reg13_output,
		line14 => reg14_output, line15 => reg15_output, line16 => reg16_output, line17 => reg17_output,
		line18 => reg18_output, line19 => reg19_output, line1a => reg1a_output, line1b => reg1b_output,
		line1c => reg1c_output, line1d => reg1d_output, line1e => reg1e_output, line1f => reg1f_output,
		output => b_data);

end Behavioral;

