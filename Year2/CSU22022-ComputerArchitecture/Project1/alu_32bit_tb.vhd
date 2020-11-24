library ieee;
use ieee.std_logic_1164.all;

entity alu_32bit_tb is
end alu_32bit_tb;

architecture Behavior of alu_32bit_tb is
	component alu_32bit
		port(
			g_select         : in std_logic_vector(3 downto 0);
			a_input, b_input : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0);
			carry            : out std_logic);
	end component;

	signal g_select : std_logic_vector(3 downto 0);
	signal a_input, b_input, output : std_logic_vector(31 downto 0);
	signal carry : std_logic;

begin
	uut: alu_32bit port map(g_select => g_select, a_input => a_input, b_input => b_input, output => output, carry => carry);
	stim_proc: process
	begin

		wait;
	end process;
end;

