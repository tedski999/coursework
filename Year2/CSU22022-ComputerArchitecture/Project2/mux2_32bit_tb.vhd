library ieee;
use ieee.std_logic_1164.all;

entity mux2_32bit_tb is
end mux2_32bit_tb;

architecture Behavior of mux2_32bit_tb is
	component mux2_32bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal line_select  : std_logic;
	signal line0, line1 : std_logic_vector(31 downto 0);
	signal output       : std_logic_vector(31 downto 0);

begin
	uut: mux2_32bit port map(line_select => line_select, line0 => line0, line1 => line1, output => output);
	stim_proc: process
	begin

		-- Test #1 - Switching selected line
		line0 <= x"00000000";
		line1 <= x"ffffffff";
		line_select <= '0'; wait for 25 ns; assert output = x"00000000" report "mux2_32bit test #1.0" severity failure;
		line_select <= '1'; wait for 25 ns; assert output = x"ffffffff" report "mux2_32bit test #1.1" severity failure;
	
		-- Test #2 - Changing lines input
		line_select <= '0';
		line0 <= x"f0f0f0f0"; wait for 25 ns; assert output = x"f0f0f0f0" report "mux2_32bit test #2.0" severity failure;
		line0 <= x"ff00ff00"; wait for 25 ns; assert output = x"ff00ff00" report "mux2_32bit test #2.1" severity failure;

		std.env.finish;
		
	end process;
end;

