library ieee;
use ieee.std_logic_1164.all;

entity extend_10to32 is
	port(
		input  : in std_logic_vector(9 downto 0);
		output : out std_logic_vector(31 downto 0));
end extend_10to32;

architecture Behavioral of extend_10to32 is
begin
	-- Extends input with msb
	output <= (31 downto input'length => input(9)) & input after 5 ns;
end Behavioral;

