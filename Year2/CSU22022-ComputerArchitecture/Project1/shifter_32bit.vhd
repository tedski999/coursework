library ieee;
use ieee.std_logic_1164.all;

entity shifter_32bit is
	port(
		left, right : in std_logic;
		operation   : in std_logic_vector(1 downto 0);
		input       : in std_logic_vector(31 downto 0);
		output      : out std_logic_vector(31 downto 0));
end shifter_32bit;

architecture Behavioral of shifter_32bit is

	component mux3_1bit
		port(
			line_select         : in std_logic_vector(1 downto 0);
			line0, line1, line2 : in std_logic;
			output              : out std_logic);
	end component;

begin

	mux00: mux3_1bit port map(line_select => operation, line0 => input(0),  line1 => input(1),  line2 => left,      output => output(0));
	mux01: mux3_1bit port map(line_select => operation, line0 => input(1),  line1 => input(2),  line2 => input(0),  output => output(1));
	mux02: mux3_1bit port map(line_select => operation, line0 => input(2),  line1 => input(3),  line2 => input(1),  output => output(2));
	mux03: mux3_1bit port map(line_select => operation, line0 => input(3),  line1 => input(4),  line2 => input(2),  output => output(3));
	mux04: mux3_1bit port map(line_select => operation, line0 => input(4),  line1 => input(5),  line2 => input(3),  output => output(4));
	mux05: mux3_1bit port map(line_select => operation, line0 => input(5),  line1 => input(6),  line2 => input(4),  output => output(5));
	mux06: mux3_1bit port map(line_select => operation, line0 => input(6),  line1 => input(7),  line2 => input(5),  output => output(6));
	mux07: mux3_1bit port map(line_select => operation, line0 => input(7),  line1 => input(8),  line2 => input(6),  output => output(7));
	mux08: mux3_1bit port map(line_select => operation, line0 => input(8),  line1 => input(9),  line2 => input(7),  output => output(8));
	mux09: mux3_1bit port map(line_select => operation, line0 => input(9),  line1 => input(10), line2 => input(8),  output => output(9));
	mux0a: mux3_1bit port map(line_select => operation, line0 => input(10), line1 => input(11), line2 => input(9),  output => output(10));
	mux0b: mux3_1bit port map(line_select => operation, line0 => input(11), line1 => input(12), line2 => input(10), output => output(11));
	mux0c: mux3_1bit port map(line_select => operation, line0 => input(12), line1 => input(13), line2 => input(11), output => output(12));
	mux0d: mux3_1bit port map(line_select => operation, line0 => input(13), line1 => input(14), line2 => input(12), output => output(13));
	mux0e: mux3_1bit port map(line_select => operation, line0 => input(14), line1 => input(15), line2 => input(13), output => output(14));
	mux0f: mux3_1bit port map(line_select => operation, line0 => input(15), line1 => input(16), line2 => input(14), output => output(15));
	mux10: mux3_1bit port map(line_select => operation, line0 => input(16), line1 => input(17), line2 => input(15), output => output(16));
	mux11: mux3_1bit port map(line_select => operation, line0 => input(17), line1 => input(18), line2 => input(16), output => output(17));
	mux12: mux3_1bit port map(line_select => operation, line0 => input(18), line1 => input(19), line2 => input(17), output => output(18));
	mux13: mux3_1bit port map(line_select => operation, line0 => input(19), line1 => input(20), line2 => input(18), output => output(19));
	mux14: mux3_1bit port map(line_select => operation, line0 => input(20), line1 => input(21), line2 => input(19), output => output(20));
	mux15: mux3_1bit port map(line_select => operation, line0 => input(21), line1 => input(22), line2 => input(20), output => output(21));
	mux16: mux3_1bit port map(line_select => operation, line0 => input(22), line1 => input(23), line2 => input(21), output => output(22));
	mux17: mux3_1bit port map(line_select => operation, line0 => input(23), line1 => input(24), line2 => input(22), output => output(23));
	mux18: mux3_1bit port map(line_select => operation, line0 => input(24), line1 => input(25), line2 => input(23), output => output(24));
	mux19: mux3_1bit port map(line_select => operation, line0 => input(25), line1 => input(26), line2 => input(24), output => output(25));
	mux1a: mux3_1bit port map(line_select => operation, line0 => input(26), line1 => input(27), line2 => input(25), output => output(26));
	mux1b: mux3_1bit port map(line_select => operation, line0 => input(27), line1 => input(28), line2 => input(26), output => output(27));
	mux1c: mux3_1bit port map(line_select => operation, line0 => input(28), line1 => input(29), line2 => input(27), output => output(28));
	mux1d: mux3_1bit port map(line_select => operation, line0 => input(29), line1 => input(30), line2 => input(28), output => output(29));
	mux1e: mux3_1bit port map(line_select => operation, line0 => input(30), line1 => input(31), line2 => input(29), output => output(30));
	mux1f: mux3_1bit port map(line_select => operation, line0 => input(31), line1 => right,     line2 => input(30), output => output(31));

end Behavioral;


