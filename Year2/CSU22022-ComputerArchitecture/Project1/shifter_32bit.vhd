library ieee;
use ieee.std_logic_1164.all;

entity shifter_32bit is
	port(
		h_select : in std_logic_vector(1 downto 0);
		input    : in std_logic_vector(31 downto 0);
		output   : out std_logic_vector(31 downto 0));
end shifter_32bit;

architecture Behavioral of shifter_32bit is

begin


end Behavioral;


