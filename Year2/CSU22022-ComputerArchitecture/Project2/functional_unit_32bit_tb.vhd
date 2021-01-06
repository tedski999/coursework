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
			vcnz             : out std_logic_vector(3 downto 0));
	end component;

	signal function_select  : std_logic_vector(4 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output           : std_logic_vector(31 downto 0);
	signal vcnz             : std_logic_vector(3 downto 0);

begin
	uut: functional_unit_32bit port map(
		function_select => function_select,
		a_input => a_input, b_input => b_input,
		output => output, vcnz => vcnz);
	stim_proc: process
	begin

		-- Test #1 - Transfer of 0
		a_input <= x"00000000"; b_input <= x"00000000"; function_select <= "00000"; wait for 500 ns;
		assert output = x"00000000" report "functional_unit_32bit test #1 - output" severity failure;
		assert vcnz = "0001"        report "functional_unit_32bit test #1 - vcnz"   severity failure;

		-- Test #2 - Addition of 3 and 5
		a_input <= x"00000003"; b_input <= x"00000005"; function_select <= "00010"; wait for 500 ns;
		assert output = x"00000008" report "functional_unit_32bit test #2 - output" severity failure;
		assert vcnz = "0000"        report "functional_unit_32bit test #2 - vcnz"   severity failure;

		-- Test #3 - Addition with carry of 0x7fffffff and 0x00000000
		a_input <= x"7fffffff"; b_input <= x"00000000"; function_select <= "00011"; wait for 500 ns;
		assert output = x"80000000" report "functional_unit_32bit test #3 - output" severity failure;
		assert vcnz = "1010"        report "functional_unit_32bit test #3 - vcnz"   severity failure;

		-- Test #4 - Decrement of 1
		a_input <= x"00000001"; b_input <= x"00000000"; function_select <= "00110"; wait for 500 ns;
		assert output = x"00000000" report "functional_unit_32bit test #4 - output" severity failure;
		assert vcnz = "0101"        report "functional_unit_32bit test #4 - vcnz"   severity failure;

		-- Test #5 - AND logic operation
		a_input <= x"0000000a"; b_input <= x"0000000c"; function_select <= "01000"; wait for 500 ns;
		assert output = x"00000008" report "functional_unit_32bit test #5" severity failure;

		-- Test #6 - NOT logic operation
		a_input <= x"00000000"; b_input <= x"00000000"; function_select <= "01110"; wait for 500 ns;
		assert output = x"ffffffff" report "functional_unit_32bit test #6" severity failure;

		-- Test #7 - Shift left
		a_input <= x"00000000"; b_input <= x"80010001"; function_select <= "11000"; wait for 500 ns;
		assert output = x"00020002" report "functional_unit_32bit test #7" severity failure;

		std.env.finish;

	end process;
end;

