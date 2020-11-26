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
	
		-- Initialize the 3 input lines
		line0 <= '0';
		line1 <= '1';
		line2 <= '1';
		
		-- Select line0
		line_select <= "00";
		wait for 10 ns;

		-- Select line1
		line_select <= "01";
		wait for 10 ns;

		-- Select line2
		line_select <= "10";
		wait for 10 ns;

		-- Change input of line2 to verify selection
		line2 <= not line2;
		wait for 10 ns;

		-- Select invalid line, this should resolve to line0
		line_select <= "11";
		wait for 10 ns;

		-- Change input of line0 to verify
		line0 <= not line0;
		wait;
		
	end process;
end;

