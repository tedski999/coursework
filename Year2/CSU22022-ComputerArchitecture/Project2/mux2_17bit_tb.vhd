library ieee;
use ieee.std_logic_1164.all;

entity mux2_17bit_tb is
end mux2_17bit_tb;

architecture Behavior of mux2_17bit_tb is
	component mux2_17bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic_vector(16 downto 0);
			output       : out std_logic_vector(16 downto 0));
	end component;

	signal line_select  : std_logic;
	signal line0, line1 : std_logic_vector(16 downto 0);
	signal output       : std_logic_vector(16 downto 0);

begin
	uut: mux2_17bit port map(line_select => line_select, line0 => line0, line1 => line1, output => output);
	stim_proc: process
	begin

		-- Test #1 - Switching selected line
		line0 <= "00000000000000000";
		line1 <= "11111111111111111";
		line_select <= '0'; wait for 25 ns; assert output = "00000000000000000" report "mux2_17bit test #1.0" severity failure;
		line_select <= '1'; wait for 25 ns; assert output = "11111111111111111" report "mux2_17bit test #1.1" severity failure;
	
		-- Test #2 - Changing lines input
		line_select <= '0';
		line0 <= "11001100110011001"; wait for 25 ns; assert output = "11001100110011001" report "mux2_17bit test #2.0" severity failure;
		line0 <= "11110000111100001"; wait for 25 ns; assert output = "11110000111100001" report "mux2_17bit test #2.1" severity failure;

		std.env.finish;
		
	end process;
end;

