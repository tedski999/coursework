library ieee;
use ieee.std_logic_1164.all;

entity logic_unit_32bit is
	port(
		operation        : in std_logic_vector(1 downto 0);
		a_input, b_input : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0));
end logic_unit_32bit;

architecture Behavioral of logic_unit_32bit is
		
	component mux4_32bit
		port(
			line_select  : in std_logic_vector(1 downto 0);
			line0, line1 : in std_logic_vector(31 downto 0);
			line2, line3 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal AND_outputs, OR_outputs  : std_logic_vector(31 downto 0);
	signal XOR_outputs, NOT_outputs : std_logic_vector(31 downto 0);

begin

	AND_outputs <= a_input and b_input after 5 ns;
	OR_outputs  <= a_input or  b_input after 5 ns;
	XOR_outputs <= a_input xor b_input after 5 ns;
	NOT_outputs <= not a_input after 5 ns;

	output_mux: mux4_32bit port map(
		line_select => operation,
		line0 => AND_outputs, line1 => OR_outputs,
		line2 => XOR_outputs, line3 => NOT_outputs,
		output => output);

end Behavioral;

