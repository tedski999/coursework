library ieee;
use ieee.std_logic_1164.all;

entity mux3_1bit is
	port(
		line_select         : in std_logic_vector(1 downto 0);
		line0, line1, line2 : in std_logic;
		output              : out std_logic);
end mux3_1bit;

architecture Behavioral of mux3_1bit is
begin
	process(line_select, line0, line1, line2)
	begin
		case line_select is
			when "00"   => output <= line0 after 5 ns;
			when "01"   => output <= line1 after 5 ns;
			when "10"   => output <= line2 after 5 ns;
			when others => output <= line0 after 5 ns;
		end case;
	end process;
end Behavioral;

