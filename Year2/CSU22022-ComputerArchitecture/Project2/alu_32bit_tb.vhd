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

		-- Test #1 - TRANSFER operation
		operation <= "0000";
		a_input <= x"00000001";
		wait for 500 ns;
		assert output = x"00000001" report "alu_32bit test #1 - output"   severity failure;
		assert carry = '0'          report "alu_32bit test #1 - carry"    severity failure;
		assert overflow = '0'       report "alu_32bit test #1 - overflow" severity failure;

		-- Test #2 - ADDITION operation
		operation <= "0010";
		a_input <= x"00000003";
		b_input <= x"00000005";
		wait for 500 ns;
		assert output = x"00000008" report "alu_32bit test #2 - output"   severity failure;
		assert carry = '0'          report "alu_32bit test #2 - carry"    severity failure;
		assert overflow = '0'       report "alu_32bit test #2 - overflow" severity failure;

		-- Test #3 - SUBTRACTION operation
		operation <= "0101";
		a_input <= x"0000002a";
		b_input <= x"0000000d";
		wait for 500 ns;
		assert output = x"0000001d" report "alu_32bit test #3 - output"   severity failure;
		assert carry = '1'          report "alu_32bit test #3 - carry"    severity failure;
		assert overflow = '0'       report "alu_32bit test #3 - overflow" severity failure;

		-- Test #4 - Logical AND operation
		operation <= "1000";
		a_input <= "10101010101010101010101010101010";
		b_input <= "11001100110011001100110011001100";
		wait for 500 ns;
		assert output = "10001000100010001000100010001000" report "alu_32bit test #4" severity failure;

		-- Test #5 - Logical NOT operation
		operation <= "1110";
		a_input <= x"ffffffff";
		wait for 500 ns;
		assert output = x"00000000" report "alu_32bit test #5" severity failure;

		-- Test #6 - Demonstrating the oVerflow flag
		operation <= "0010";
		a_input <= x"7fffffff"; -- 0111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		assert overflow = '1' report "alu_32bit test #6" severity failure;
		
		-- Test #7 - Demonstrating the Carry flag
		operation <= "0010";
		a_input <= x"ffffffff"; -- 1111...1111
		b_input <= x"00000001"; -- 0000...0001
		wait for 500 ns;
		assert carry = '1' report "alu_32bit test #7" severity failure;
		
		std.env.finish;

	end process;
end;

