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

		-- Addition of 1 and 2. Expected:
		-- output: 0x00000011
		-- vcnz: 0000
		a_input <= x"00000001";
		b_input <= x"00000002";
		function_select <= "00000";
		wait for 50 ns;

		-- Subtraction of 5 and 5. Expected:
		-- output: 0x00000000
		-- vcnz: 0001
		a_input <= x"00000005";
		b_input <= x"00000005";
		function_select <= "00001";
		wait for 50 ns;
		
		-- Addition of ? and ?. Expected:
		-- output: 0x10000000
		-- vcnz: 1010
		a_input <= x"01000000";
		b_input <= x"01000000";
		function_select <= "00001";
		wait for 50 ns;

		-- Addition of -1 and 1. Expected:
		-- output: 0x00000000
		-- vcnz: 0101
		a_input <= x"ffffffff";
		b_input <= x"00000001";
		function_select <= "00000";
		wait for 50 ns;

		-- TODO: test logic gates

		-- Shift left of 1. Expected:
		-- output: 0x00800002
		b_input <= x"00400001";
		function_select <= "00000";
		wait for 50 ns;

		-- Shift right of 2. Expected:
		-- output: 0x00400001
		b_input <= x"00800002";
		function_select <= "00000";
		wait;

	end process;
end;

