library ieee;
use ieee.std_logic_1164.all;

entity datapath_32bit is
	port(
		clock, load_enable, use_immediate, load_from_mem   : in std_logic;
		function_select, dst_address, a_address, b_address : in std_logic_vector(4 downto 0);
		constant_in, input_data                            : in std_logic_vector(31 downto 0);
		address_out, data_out                              : out std_logic_vector(31 downto 0);
		v, c, n, z                                         : out std_logic);
end datapath_32bit;

architecture Behavioral of datapath_32bit is

	component register_file_32bit
		port(
			clock, load_enable             : in std_logic;
			dst_select, a_select, b_select : in std_logic_vector(4 downto 0);
			input_data                     : in std_logic_vector(31 downto 0);
			a_output_data, b_output_data   : out std_logic_vector(31 downto 0));
	end component;

	component functional_unit_32bit
		port(
			function_select  : in std_logic_vector(4 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			v, c, n, z       : out std_logic);
	end component;

	component mux2_32bit
		port(
			line_select  : in  std_logic;
			line0, line1 : in  std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal a_bus, b_output_data, b_bus : std_logic_vector(31 downto 0);
	signal functional_unit_output, loopback_data : std_logic_vector(31 downto 0);

begin

	-- The 32x32-bit register file
	register_file: register_file_32bit port map(
		clock => clock, load_enable => load_enable,
		dst_select => dst_address, a_select => a_address, b_select => b_address,
		input_data => loopback_data,
		a_output_data => a_bus, b_output_data => b_output_data);

	-- The 32-bit functional unit, with ALU and Shifter
	functional_unit: functional_unit_32bit port map(
		function_select => function_select,
		a_input => a_bus, b_input => b_bus,
		output => functional_unit_output,
		v => v, c => c, n => n, z => z);

	-- Multiplexer for selecting functional unit B input between bus B and an immediate value
	mux_b: mux2_32bit port map(
		line_select => use_immediate,
		line0 => b_output_data, line1 => constant_in,
		output => b_bus);

	-- Multiplexer for selecting input to dst register between functional unit output and input_data
	mux_d: mux2_32bit port map(
		line_select => load_from_mem,
		line0 => functional_unit_output, line1 => input_data,
		output => loopback_data);
	
	process(a_bus, b_bus)
	begin
		-- Output the A and B buses to address_out and data_out
		address_out <= a_bus;
		data_out <= b_bus;
	end process;

end Behavioral;

