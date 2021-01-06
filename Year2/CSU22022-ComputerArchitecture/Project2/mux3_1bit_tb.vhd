library ieee;
use ieee.std_logic_1164.all;

entity mux3_1bit_tb is
end mux3_1bit_tb;

architecture Behavior of mux3_1bit_tb is
	component mux3_1bit
		port(
			line_select         : in std_logic_vector(1 downto 0);
			line0, line1, line2 : in std_logic;
			output              : out std_logic);
	end component;

	signal line_select         : std_logic_vector(1 downto 0);
	signal line0, line1, line2 : std_logic;
	signal output              : std_logic;

begin
	uut: mux3_1bit port map(line_select => line_select, line0 => line0, line1 => line1, line2 => line2, output => output);
	stim_proc: process
	begin
	
		-- Test #1 - Switching selected line
		line0 <= '0'; line1 <= '1'; line2 <= '0';
		line_select <= "00"; wait for 25 ns; assert output = '0' report "mux3_1bit test #1.0" severity failure;
		line_select <= "01"; wait for 25 ns; assert output = '1' report "mux3_1bit test #1.1" severity failure;
		line_select <= "10"; wait for 25 ns; assert output = '0' report "mux3_1bit test #1.2" severity failure;
		line_select <= "11"; wait for 25 ns; assert output = '0' report "mux3_1bit test #1.3" severity failure;

		-- Test #2 - Switching selected line with a different input
		line0 <= '0'; line1 <= '0'; line2 <= '1';
		line_select <= "00"; wait for 25 ns; assert output = '0' report "mux3_1bit test #2.0" severity failure;
		line_select <= "01"; wait for 25 ns; assert output = '0' report "mux3_1bit test #2.1" severity failure;
		line_select <= "10"; wait for 25 ns; assert output = '1' report "mux3_1bit test #2.2" severity failure;
		line_select <= "11"; wait for 25 ns; assert output = '0' report "mux3_1bit test #2.3" severity failure;

		-- Test #3 - Changing lines input
		line_select <= "00";
		line0 <= '0'; wait for 25 ns; assert output = '0' report "mux3_1bit test #3.0" severity failure;
		line0 <= '1'; wait for 25 ns; assert output = '1' report "mux3_1bit test #3.1" severity failure;

		std.env.finish;
		
	end process;
end;

