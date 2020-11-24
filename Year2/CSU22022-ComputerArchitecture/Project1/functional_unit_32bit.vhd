library ieee;
use ieee.std_logic_1164.all;

entity functional_unit_32bit is
	port(
		function_select  : in std_logic_vector(4 downto 0);
		a_input, b_input : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0);
		v, c, n, z       : out std_logic);
end functional_unit_32bit;

architecture Behavioral of functional_unit_32bit is

	component alu_32bit
		port(
			operation        : in std_logic_vector(3 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry            : out std_logic);
	end component;
	
	component shifter_32bit
		port(
			left, right : in std_logic;
			operation   : in std_logic_vector(1 downto 0);
			input       : in std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	component mux2_32bit
		port(
			line_select  : in  std_logic;
			line0, line1 : in  std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal alu_output, shifter_output : std_logic_vector(31 downto 0);

begin
	-- FIXME: i guessed the function_select bit
	alu: alu_32bit port map(operation => function_select(3 downto 0), a_input => a_input, b_input => b_input, output => alu_output, carry => c);
	shifter: shifter_32bit port map(operation => function_select(1 downto 0), left => '0', right => '0', input => b_input, output => shifter_output); -- NOTE: the left/right inputs are tied low
	output_mux: mux2_32bit port map(line_select => function_select(4), line0 => alu_output, line1 => shifter_output, output => output);

	process(alu_output)
	begin
		-- FIXME: guessed the bits
		-- v is set if the two inputs have the same sign, but the output has a different sign
		v <= not (a_input(31) xor b_input(31)) and (a_input(31) xor alu_output(31)) after 5 ns;

		-- FIXME: guessed the bits
		-- n is set if the msb is set in the output
		n <= alu_output(31) after 5 ns;

		 -- z is set if the output is zero
		if alu_output = x"00000000" then
			z <= '1' after 5 ns;
		else
			z <= '0' after 5 ns;
		end if;
	end process;
end Behavioral;

