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

		-- Test #1 - TRANSFER: F = A
		operation <= "000"; a_input <= x"12345678"; b_input <= x"00000000"; wait for 500 ns;
		assert output = x"12345678" report "arithmetic_unit_32bit test #1 - output"   severity failure;

		-- Test #2 - INCREMENT: F = A + 1
		operation <= "001"; a_input <= x"00000001"; b_input <= x"00000000"; wait for 500 ns;
		assert output = x"00000002" report "arithmetic_unit_32bit test #2 - output"   severity failure;
		assert carry = '0'          report "arithmetic_unit_32bit test #2 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #2 - overflow" severity failure;

		-- Test #3 - ADDITION: F = A + B
		operation <= "010"; a_input <= x"00000005"; b_input <= x"00000006"; wait for 500 ns;
		assert output = x"0000000b" report "arithmetic_unit_32bit test #3 - output"   severity failure;
		assert carry = '0'          report "arithmetic_unit_32bit test #3 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #3 - overflow" severity failure;

		-- Test #4 - ADDITION W/ CARRY: F = A + B + 1
		operation <= "011"; a_input <= x"00000005"; b_input <= x"00000006"; wait for 500 ns;
		assert output = x"0000000c" report "arithmetic_unit_32bit test #4 - output"   severity failure;
		assert carry = '0'          report "arithmetic_unit_32bit test #4 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #4 - overflow" severity failure;
		
		-- Test #5 - ADDITION W/ B's 1's C.B: F = A + ~B
		operation <= "100"; a_input <= x"00000004"; b_input <= x"00000003"; wait for 500 ns;
		assert output = x"00000000" report "arithmetic_unit_32bit test #5 - output"   severity failure;
		assert carry = '1'          report "arithmetic_unit_32bit test #5 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #5 - overflow" severity failure;
		
		-- Test #6 - SUBTRACTION: F = A - B
		operation <= "101"; a_input <= x"00000004"; b_input <= x"00000003"; wait for 500 ns;
		assert output = x"00000001" report "arithmetic_unit_32bit test #6 - output"   severity failure;
		assert carry = '1'          report "arithmetic_unit_32bit test #6 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #6 - overflow" severity failure;

		-- Test #7 - DECREMENT: F = A - 1
		operation <= "110"; a_input <= x"00000006"; b_input <= x"00000000"; wait for 500 ns;
		assert output = x"00000005" report "arithmetic_unit_32bit test #7 - output"   severity failure;
		assert carry = '1'          report "arithmetic_unit_32bit test #7 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #7 - overflow" severity failure;

		-- Test #8 - TRANSFER: F = A
		operation <= "111"; a_input <= x"00000004"; wait for 500 ns;
		assert output = x"00000004" report "arithmetic_unit_32bit test #8 - output"   severity failure;
		
		-- Test #9 - Demonstrating the oVerflow flag
		operation <= "010"; a_input <= x"7fffffff"; b_input <= x"00000001"; wait for 500 ns;
		assert output = x"80000000" report "arithmetic_unit_32bit test #9 - output"   severity failure;
		assert carry = '0'          report "arithmetic_unit_32bit test #9 - carry"    severity failure;
		assert overflow = '1'       report "arithmetic_unit_32bit test #9 - overflow" severity failure;
		
		-- Test #10 - Demonstrating the Carry flag
		operation <= "010"; a_input <= x"ffffffff"; b_input <= x"00000001"; wait for 500 ns;
		assert output = x"00000000" report "arithmetic_unit_32bit test #10 - output"   severity failure;
		assert carry = '1'          report "arithmetic_unit_32bit test #10 - carry"    severity failure;
		assert overflow = '0'       report "arithmetic_unit_32bit test #10 - overflow" severity failure;
		
		std.env.finish;

	end process;
end;

