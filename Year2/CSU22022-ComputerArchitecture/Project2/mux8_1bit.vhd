library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux8_1bit is
	port(
		line_select : in std_logic_vector(2 downto 0);
		lines       : in std_logic_vector(7 downto 0);
		output      : out std_logic);
end mux8_1bit;

architecture Behavioral of mux8_1bit is
begin
	output <= lines(to_integer(unsigned(line_select))) after 5 ns;
end Behavioral;

