library ieee;
use ieee.std_logic_1164.all;

entity mux2_1bit_tb is
end mux2_1bit_tb;

architecture Behavior of mux2_1bit_tb is
	component mux2_1bit
		port(
			line_select : in std_logic;
			lines       : in std_logic_vector(1 downto 0);
			output      : out std_logic);
	end component;

	signal line_select : std_logic;
	signal lines       : std_logic_vector(1 downto 0);
	signal output      : std_logic;

begin
	uut: mux2_1bit port map(line_select => line_select, lines => lines, output => output);
	stim_proc: process
	begin
	
		-- Test #1 - Switching selected line
		lines <= "10";
		line_select <= '0'; wait for 25 ns; assert output = '0' report "mux2_1bit test #1.0" severity failure;
		line_select <= '1'; wait for 25 ns; assert output = '1' report "mux2_1bit test #1.1" severity failure;
	
		-- Test #2 - Changing lines input
		line_select <= '0';
		lines <= "00"; wait for 25 ns; assert output = '0' report "mux2_1bit test #2.0" severity failure;
		lines <= "01"; wait for 25 ns; assert output = '1' report "mux2_1bit test #2.1" severity failure;

		std.env.finish;
		
	end process;
end;

