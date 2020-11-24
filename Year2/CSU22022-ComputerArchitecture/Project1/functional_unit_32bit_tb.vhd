library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity functional_unit_32bit_tb is
end functional_unit_32bit_tb;

architecture Behavior of functional_unit_32bit_tb is
	component functional_unit_32bit
		port(
			function_select  : in std_logic_vector(4 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			v, c, n, z       : out std_logic);
	end component;

	signal function_select : std_logic_vector(4 downto 0);
	signal a_input, b_input : std_logic_vector(31 downto 0);
	signal output : std_logic_vector(31 downto 0);
	signal v, c, n, z : std_logic;

begin
	uut: functional_unit_32bit port map(
		function_select => function_select,
		a_input => a_input, b_input => b_input,
		output => output, v => v, c => c, n => n, z => z);
	stim_proc: process
	begin
		-- TODO: check if function_select works
		-- TODO: check if vcnz works
		wait;
	end process;
end;

