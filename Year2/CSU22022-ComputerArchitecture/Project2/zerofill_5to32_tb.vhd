library ieee;
use ieee.std_logic_1164.all;

entity zerofill_5to32_tb is
end zerofill_5to32_tb;

architecture Behavior of zerofill_5to32_tb is
	component zerofill_5to32
		port(
			input  : in std_logic_vector(4 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	signal input  : std_logic_vector(4 downto 0);
	signal output : std_logic_vector(31 downto 0);

begin
	uut: zerofill_5to32 port map(input => input, output => output);
	stim_proc: process
	begin

		-- Test #1 - Pad 0
		input <= "00000"; wait for 25 ns;
		assert output = x"00000000" report "zerofill_5to32 test #1" severity failure;

		-- Test #2 - Pad 1
		input <= "00001"; wait for 25 ns;
		assert output = x"00000001" report "zerofill_5to32 test #2" severity failure;

		-- Test #3 - Pad 15
		input <= "01111"; wait for 25 ns;
		assert output = x"0000000f" report "zerofill_5to32 test #3" severity failure;

		-- Test #4 - Pad 16
		input <= "10000"; wait for 25 ns;
		assert output = x"00000010" report "zerofill_5to32 test #4" severity failure;

		-- Test #5 - Pad 31
		input <= "11111"; wait for 25 ns;
		assert output = x"0000001f" report "zerofill_5to32 test #5" severity failure;

		std.env.finish;

	end process;
end;

