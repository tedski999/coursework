library ieee;
use ieee.std_logic_1164.all;

entity mux2_1bit_tb is
end mux2_1bit_tb;

architecture Behavior of mux2_1bit_tb is
	component mux2_1bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic;
			output       : out std_logic);
	end component;

	signal line_select  : std_logic;
	signal line0, line1 : std_logic;
	signal output       : std_logic;

begin
	uut: mux2_1bit port map(line_select => line_select, line0 => line0, line1 => line1, output => output);
	stim_proc: process
	begin
	
		-- Initialize the 2 input lines to unique values
		line0 <= '0';
		line1 <= '1';
		
		-- Select line0
		line_select <= '0';
		wait for 10 ns;

		-- Select line1
		line_select <= '1';
		wait for 10 ns;

		-- Select line0 again
		line_select <= '0';
		wait for 10 ns;

		-- Change input of line0
		line0 <= '1';
		wait for 10 ns;

		-- Change input of line1
		line1 <= '0';
		wait for 10 ns;
		
		-- Select line1 again
		line_select <= '1';
		wait;
		
	end process;
end;

