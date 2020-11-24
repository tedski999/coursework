library ieee;
use ieee.std_logic_1164.all;

entity alu_32bit is
	port(
		g_select         : in std_logic_vector(3 downto 0);
		a_input, b_input : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0);
		carry            : out std_logic);
end alu_32bit;

architecture Behavioral of alu_32bit is

begin


end Behavioral;


