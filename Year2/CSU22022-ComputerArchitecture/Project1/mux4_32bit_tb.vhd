library ieee;
use ieee.std_logic_1164.all;

entity mux4_32bit_tb is
end mux4_32bit_tb;

architecture Behavior of mux4_32bit_tb is
	component mux4_32bit
		port(
			line_select  : in std_logic_vector(1 downto 0);
			line0, line1 : in std_logic_vector(31 downto 0);
			line2, line3 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal line_select  : std_logic_vector(1 downto 0);
	signal line0, line1 : std_logic_vector(31 downto 0);
	signal line2, line3 : std_logic_vector(31 downto 0);
	signal output       : std_logic_vector(31 downto 0);

begin
	uut: mux4_32bit port map(
		line_select => line_select,
		line0 => line0, line1 => line1,
		line2 => line2, line3 => line3,
		output => output);
	stim_proc: process
	begin
	
		-- Initialize the 4 input lines to unique values
		line0 <= x"000000ff";
		line1 <= x"0000ffff";
		line2 <= x"00ffffff";
		line3 <= x"ffffffff";
		
		-- Test all the lines, try changing the inputs
		line_select <= "00"; wait for 10 ns;  -- Select line0
		line_select <= "01"; wait for 10 ns;  -- Select line1
		line_select <= "10"; wait for 10 ns;  -- Select line2
		line_select <= "11"; wait for 10 ns;  -- Select line3
		line_select <= "00"; wait for 10 ns;  -- Select line0 again
		line_select <= "10"; wait for 10 ns;  -- Select line2 again
		line0 <= x"f0f0f0f0"; wait for 10 ns; -- Change input of line0
		line2 <= x"0f0f0f0f"; wait;           -- Change input of line2
		
	end process;
end;

