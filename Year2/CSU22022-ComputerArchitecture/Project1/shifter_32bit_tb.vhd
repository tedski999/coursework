library ieee;
use ieee.std_logic_1164.all;

entity shifter_32bit_tb is
end shifter_32bit_tb;

architecture Behavior of shifter_32bit_tb is
	component shifter_32bit
		port(
			h_select : in std_logic_vector(1 downto 0);
			input    : in std_logic_vector(31 downto 0);
			output   : out std_logic_vector(31 downto 0));
	end component;

	signal h_select : std_logic_vector(1 downto 0);
	signal input, output : std_logic_vector(31 downto 0);

begin
	uut: shifter_32bit port map(h_select => h_select, input => input, output => output);
	stim_proc: process
	begin

		wait;
	end process;
end;

