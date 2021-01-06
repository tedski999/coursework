library ieee;
use ieee.std_logic_1164.all;

entity mux2_1bit is
	port(
		line_select : in std_logic;
		lines       : in std_logic_vector(1 downto 0);
		output      : out std_logic);
end mux2_1bit;

architecture Behavioral of mux2_1bit is
begin
	output <= lines(0) when (line_select = '0') else lines(1) after 5 ns;
end Behavioral;

