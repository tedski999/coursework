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
			g_select         : in std_logic_vector(3 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry            : out std_logic);
	end component;
	
	component shifter_32bit
		port(
			h_select : in std_logic_vector(1 downto 0);
			input    : in std_logic_vector(31 downto 0);
			output   : out std_logic_vector(31 downto 0));
	end component;

	component mux2_32bit
		port(
			line_select  : in  std_logic;
			line0, line1 : in  std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal alu_output, shifter_output : std_logic_vector(31 downto 0);

begin
	alu: alu_32bit port map(g_select => "0000", a_input => a_input, b_input => b_input, output => alu_output, carry => c);
	shifter: shifter_32bit port map(h_select => "00", input => b_input, output => shifter_output);
	mux_f: mux2_32bit port map(line_select => '0', line0 => alu_output, line1 => shifter_output, output => output);

	process(alu_output)
	begin
		--v <= not alu_output(31) = (a_input(31) xor b_input(31));
		n <= alu_output(31);
		--if (
		--	z <= alu_output = x"00000000");
		--else
		--	z <= '0';
	end process;
end Behavioral;

