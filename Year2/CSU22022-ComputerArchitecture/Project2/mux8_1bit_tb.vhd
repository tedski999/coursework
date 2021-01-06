library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux8_1bit_tb is
end mux8_1bit_tb;

architecture Behavior of mux8_1bit_tb is
	component mux8_1bit
		port(
			line_select : in std_logic_vector(2 downto 0);
			lines       : in std_logic_vector(7 downto 0);
			output      : out std_logic);
	end component;

	signal line_select : std_logic_vector(2 downto 0);
	signal lines       : std_logic_vector(7 downto 0);
	signal output      : std_logic;

begin
	uut: mux8_1bit port map(line_select => line_select, lines => lines, output => output);
	stim_proc: process
	begin

		-- Test #1 - Switching selected line
		lines <= "10101010";
		line_select <= "000"; wait for 25 ns; assert output = '0' report "mux8_1bit test #1.0" severity failure;
		line_select <= "001"; wait for 25 ns; assert output = '1' report "mux8_1bit test #1.1" severity failure;
		line_select <= "010"; wait for 25 ns; assert output = '0' report "mux8_1bit test #1.3" severity failure;
		line_select <= "011"; wait for 25 ns; assert output = '1' report "mux8_1bit test #1.4" severity failure;
		line_select <= "100"; wait for 25 ns; assert output = '0' report "mux8_1bit test #1.5" severity failure;
		line_select <= "101"; wait for 25 ns; assert output = '1' report "mux8_1bit test #1.6" severity failure;
		line_select <= "110"; wait for 25 ns; assert output = '0' report "mux8_1bit test #1.7" severity failure;
		line_select <= "111"; wait for 25 ns; assert output = '1' report "mux8_1bit test #1.8" severity failure;

		-- Test #2 - Switching selected line with a different input
		lines <= "11001100";
		line_select <= "000"; wait for 25 ns; assert output = '0' report "mux8_1bit test #2.0" severity failure;
		line_select <= "001"; wait for 25 ns; assert output = '0' report "mux8_1bit test #2.1" severity failure;
		line_select <= "010"; wait for 25 ns; assert output = '1' report "mux8_1bit test #2.2" severity failure;
		line_select <= "011"; wait for 25 ns; assert output = '1' report "mux8_1bit test #2.3" severity failure;
		line_select <= "100"; wait for 25 ns; assert output = '0' report "mux8_1bit test #2.4" severity failure;
		line_select <= "101"; wait for 25 ns; assert output = '0' report "mux8_1bit test #2.5" severity failure;
		line_select <= "110"; wait for 25 ns; assert output = '1' report "mux8_1bit test #2.6" severity failure;
		line_select <= "111"; wait for 25 ns; assert output = '1' report "mux8_1bit test #2.7" severity failure;

		-- Test #3 - Changing lines input
		line_select <= "000";
		lines <= "00000000"; wait for 25 ns; assert output = '0' report "mux8_1bit test #3.0" severity failure;
		lines <= "00000001"; wait for 25 ns; assert output = '1' report "mux8_1bit test #3.1" severity failure;
		lines <= "00000010"; wait for 25 ns; assert output = '0' report "mux8_1bit test #3.2" severity failure;
		lines <= "00000011"; wait for 25 ns; assert output = '1' report "mux8_1bit test #3.3" severity failure;
		line_select <= "001";
		lines <= "00000000"; wait for 25 ns; assert output = '0' report "mux8_1bit test #3.4" severity failure;
		lines <= "00000001"; wait for 25 ns; assert output = '0' report "mux8_1bit test #3.5" severity failure;
		lines <= "00000010"; wait for 25 ns; assert output = '1' report "mux8_1bit test #3.6" severity failure;
		lines <= "00000011"; wait for 25 ns; assert output = '1' report "mux8_1bit test #3.7" severity failure;

		std.env.finish;

	end process;
end;

