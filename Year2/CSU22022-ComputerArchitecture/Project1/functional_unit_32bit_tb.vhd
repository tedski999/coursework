library ieee;
use ieee.std_logic_1164.all;

entity functional_unit_32bit_tb is
end functional_unit_32bit_tb;

architecture Behavior of functional_unit_32bit_tb is
	component functional_unit_32bit
		port(
			function_select  : in std_logic_vector(4 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			v, c, n, z       : out std_logic);
	end component;

	signal function_select  : std_logic_vector(4 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output           : std_logic_vector(31 downto 0);
	signal v, c, n, z       : std_logic;

begin
	uut: functional_unit_32bit port map(
		function_select => function_select,
		a_input => a_input, b_input => b_input,
		output => output, v => v, c => c, n => n, z => z);
	stim_proc: process
	begin

		-- Transfer of 0. Expected:
		-- output: 0x00000000
		-- vcnz: 0001
		a_input <= x"00000000";
		b_input <= x"00000000";
		function_select <= "00000";
		wait for 500 ns;

		-- Addition of 3 and 5. Expected:
		-- output: 0x00000008
		-- vcnz: 0000
		a_input <= x"00000003";
		b_input <= x"00000005";
		function_select <= "00010";
		wait for 500 ns;

		-- Addition w/ carry of 0x7fffffff and 0x00000000. Expected:
		-- output: 0x10000000
		-- vcnz: 1010
		a_input <= x"7fffffff";
		b_input <= x"00000000";
		function_select <= "00011";
		wait for 500 ns;

		-- Decrement of 1. Expected:
		-- output: 0x00000000
		-- vcnz: 0101
		a_input <= x"00000001";
		b_input <= x"00000000";
		function_select <= "00110";
		wait for 500 ns;

		-- AND logic operation. Expected:
		-- output: 0x00000000
		a_input <= x"0000000a";
		b_input <= x"0000000c";
		function_select <= "01000";
		wait for 500 ns;

		-- NOT logic operation. Expected:
		-- output: 0xffffffff
		a_input <= x"00000000";
		b_input <= x"00000000";
		function_select <= "01110";
		wait for 500 ns;

		-- Shift left. Expected:
		-- output: 0x00020002
		a_input <= x"00000000";
		b_input <= x"80010001";
		function_select <= "11000";
		wait;

	end process;
end;

