library ieee;
use ieee.std_logic_1164.all;

entity alu_32bit_tb is
end alu_32bit_tb;

architecture Behavior of alu_32bit_tb is
	component alu_32bit
		port(
			operation        : in std_logic_vector(3 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry, overflow  : out std_logic);
	end component;

	signal operation        : std_logic_vector(3 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output           : std_logic_vector(31 downto 0);
	signal carry, overflow  : std_logic;

begin
	uut: alu_32bit port map(
		operation => operation, a_input => a_input, b_input => b_input,
		output => output, carry => carry, overflow => overflow);
	stim_proc: process
	begin

		-- TRANSFER
		operation <= "0000";
		a_input <= x"00000001";
		wait for 500 ns;

		-- ADDITION
		operation <= "0010";
		a_input <= x"00000003";
		b_input <= x"00000005";
		wait for 500 ns;

		-- SUBTRACTION
		operation <= "0101";
		a_input <= x"0000002a";
		b_input <= x"0000000d";
		wait for 500 ns;

		-- AND
		operation <= "1000";
		a_input <= "10101010101010101010101010101010";
		b_input <= "11001100110011001100110011001100";
		wait for 500 ns;

		-- NOT
		operation <= "1110";
		a_input <= x"ffffffff";
		wait for 500 ns;

		-- Demonstrating the oVerflow flag
		operation <= "0010";
		a_input <= x"7fffffff"; -- 0111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		
		-- Demonstrating the Carry flag
		operation <= "0010";
		a_input <= x"ffffffff"; -- 1111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		
		-- Returning to zero
		operation <= "0000";
		a_input <= x"00000000";
		b_input <= x"00000000";
		wait;

	end process;
end;

