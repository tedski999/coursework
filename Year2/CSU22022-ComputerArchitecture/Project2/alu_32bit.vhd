library ieee;
use ieee.std_logic_1164.all;

entity alu_32bit is
	port(
		operation        : in std_logic_vector(3 downto 0);
		a_input, b_input : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0);
		carry, overflow  : out std_logic);
end alu_32bit;

architecture Behavioral of alu_32bit is

	component arithmetic_unit_32bit
		port(
			operation        : in std_logic_vector(2 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry, overflow  : out std_logic);
	end component;

	component logic_unit_32bit
		port(
			operation        : in std_logic_vector(1 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0));
	end component;

	component mux2_32bit
		port(
			line_select  : in std_logic;
			line0, line1 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal arithmetic_output, logic_output: std_logic_vector(31 downto 0);

begin

	arithmetic_unit: arithmetic_unit_32bit port map(
		operation => operation(2 downto 0),
		a_input => a_input, b_input => b_input,
		output => arithmetic_output, carry => carry, overflow => overflow);

	logic_unit: logic_unit_32bit port map(
		operation => operation(2 downto 1),
		a_input => a_input, b_input => b_input,
		output => logic_output);

	output_select_mux: mux2_32bit port map(
		line_select => operation(3),
		line0 => arithmetic_output, line1 => logic_output,
		output => output);

end Behavioral;

