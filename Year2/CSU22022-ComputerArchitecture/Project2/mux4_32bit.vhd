library ieee;
use ieee.std_logic_1164.all;

entity mux4_32bit is
	port(
		line_select  : in std_logic_vector(1 downto 0);
		line0, line1 : in std_logic_vector(31 downto 0);
		line2, line3 : in std_logic_vector(31 downto 0);
		output       : out std_logic_vector(31 downto 0));
end mux4_32bit;

architecture Behavioral of mux4_32bit is
begin

	process(line_select, line0, line1, line2, line3)
	begin

		-- Set the output to the approptiate input line
		case line_select is
			when "00"   => output <= line0 after 5 ns;
			when "01"   => output <= line1 after 5 ns;
			when "10"   => output <= line2 after 5 ns;
			when "11"   => output <= line3 after 5 ns;
			when others => output <= x"00000000" after 5 ns;
		end case;

	end process;

end Behavioral;

