library ieee;
use ieee.std_logic_1164.all;

entity zerofill_5to32 is
	port(
		input  : in std_logic_vector(4 downto 0);
		output : out std_logic_vector(31 downto 0));
end zerofill_5to32;

architecture Behavioral of zerofill_5to32 is
begin
	-- Pads input with leading zeros
	output <= (31 downto input'length => '0') & input after 5 ns;
end Behavioral;

