library ieee;
use ieee.std_logic_1164.all;

entity arithmetic_unit_32bit_tb is
end arithmetic_unit_32bit_tb;

architecture Behavior of arithmetic_unit_32bit_tb is
	component arithmetic_unit_32bit
		port(
			operation        : in std_logic_vector(2 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry, overflow  : out std_logic);
	end component;

	signal operation        : std_logic_vector(2 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output           : std_logic_vector(31 downto 0);
	signal carry, overflow  : std_logic;

begin
	uut: arithmetic_unit_32bit port map(
		operation => operation,
		a_input => a_input, b_input => b_input,
		output => output, carry => carry, overflow => overflow);
	stim_proc: process
	begin

		-- NOTE: The arithmetic unit has a maximum total gate delay of 480 ns

		-- Operation 000 (TRANSFER): F = A
		operation <= "000";
		a_input <= x"12345678";
		wait for 500 ns;

		-- Operation 001 (INCREMENT): F = A + 1
		operation <= "001";
		a_input <= x"00000001";
		wait for 500 ns;

		-- Operation 010 (ADDITION): F = A + B
		operation <= "010";
		a_input <= x"00000005";
		b_input <= x"00000006";
		wait for 500 ns;

		-- Operation 011 (ADDITION W/ CARRY): F = A + B + 1
		operation <= "011";
		a_input <= x"00000005";
		b_input <= x"00000006";
		wait for 500 ns;
		
		-- Operation 100 (ADDITION W/ B's 1's C.B): F = A + ~B
		operation <= "100";
		a_input <= x"00000004";
		b_input <= x"00000003";
		wait for 500 ns;
		
		-- Operation 100 (SUBTRACTION): F = A + -B + 1 = A - B
		operation <= "101";
		a_input <= x"00000004";
		b_input <= x"00000003";
		wait for 500 ns;

		-- Operation 100 (DECREMENT): F = A + -1 = A - 1
		operation <= "110";
		a_input <= x"00000006";
		wait for 500 ns;

		-- Operation 100 (TRANSFER): F = A - 1 + 1 = A
		operation <= "111";
		a_input <= x"00000004";
		wait for 500 ns;
		
		-- Demonstrating the oVerflow flag
		operation <= "010";
		a_input <= x"7fffffff"; -- 0111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		
		-- Demonstrating the Carry flag
		operation <= "010";
		a_input <= x"ffffffff"; -- 1111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		
		-- Returning to zero
		operation <= "000";
		a_input <= x"00000000";
		b_input <= x"00000000";
		wait;

	end process;
end;

