library ieee;
use ieee.std_logic_1164.all;

entity mux2_17bit is
	port(
		line_select  : in std_logic;
		line0, line1 : in std_logic_vector(16 downto 0);
		output       : out std_logic_vector(16 downto 0));
end mux2_17bit;

architecture Behavioral of mux2_17bit is
begin
	output <= line0 when (line_select = '0') else line1;
end Behavioral;

