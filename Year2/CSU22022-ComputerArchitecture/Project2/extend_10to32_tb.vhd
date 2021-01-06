library ieee;
use ieee.std_logic_1164.all;

entity extend_10to32_tb is
end extend_10to32_tb;

architecture Behavior of extend_10to32_tb is
	component extend_10to32
		port(
			input  : in std_logic_vector(9 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	signal input  : std_logic_vector(9 downto 0);
	signal output : std_logic_vector(31 downto 0);

begin
	uut: extend_10to32 port map(input => input, output => output);
	stim_proc: process
	begin

		-- Test #1 - Extend 0
		input <= "0000000000"; wait for 25 ns;
		assert output = x"00000000" report "extend_10to32 test #1" severity failure;

		-- Test #2 - Extend 1
		input <= "0000000001"; wait for 25 ns;
		assert output = x"00000001" report "extend_10to32 test #2" severity failure;

		-- Test #3 - Extend 511
		input <= "0111111111"; wait for 25 ns;
		assert output = x"000001ff" report "extend_10to32 test #3" severity failure;

		-- Test #4 - Extend -512
		input <= "1000000000"; wait for 25 ns;
		assert output = x"fffffe00" report "extend_10to32 test #4" severity failure;

		-- Test #5 - Extend -1
		input <= "1111111111"; wait for 25 ns;
		assert output = x"ffffffff" report "extend_10to32 test #5" severity failure;

		std.env.finish;
	end process;
end;

