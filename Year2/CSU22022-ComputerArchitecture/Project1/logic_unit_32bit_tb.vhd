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
		-- The 4 lsb of a_input and b_input cover every logic case
		a_input <= "00000000000000000000000000001010";
		b_input <= "00000000000000000000000000001100";
		operation <= "00"; wait for 50 ns; -- AND result: ...1000
		operation <= "01"; wait for 50 ns; -- OR  result: ...1110
		operation <= "10"; wait for 50 ns; -- XOR result: ...0110
		operation <= "11"; wait;           -- NOT result: ...0101
	end process;
end;

