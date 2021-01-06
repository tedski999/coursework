library ieee;
use ieee.std_logic_1164.all;

entity logic_unit_32bit_tb is
end logic_unit_32bit_tb;

architecture Behavior of logic_unit_32bit_tb is
	component logic_unit_32bit
		port(
			operation        : in std_logic_vector(1 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0));
	end component;

	signal operation        : std_logic_vector(1 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output           : std_logic_vector(31 downto 0);

begin
	uut: logic_unit_32bit port map(operation => operation, a_input => a_input, b_input => b_input, output => output);
	stim_proc: process
	begin

		-- The 4 lsb of a_input and b_input cover every logical operation case
		a_input <= "00000000000000000000000000001010";
		b_input <= "00000000000000000000000000001100";

		-- Tests - Logical operations
		operation <= "00"; wait for 25 ns; assert output = "00000000000000000000000000001000" report "logic_unit_32bit test #1" severity failure;
		operation <= "01"; wait for 25 ns; assert output = "00000000000000000000000000001110" report "logic_unit_32bit test #2" severity failure;
		operation <= "10"; wait for 25 ns; assert output = "00000000000000000000000000000110" report "logic_unit_32bit test #3" severity failure;
		operation <= "11"; wait for 25 ns; assert output = "11111111111111111111111111110101" report "logic_unit_32bit test #4" severity failure;

		std.env.finish;

	end process;
end;

