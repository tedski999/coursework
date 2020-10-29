library ieee;
use ieee.std_logic_1164.all;

entity mux2_32 is
	port(
		s        : in  std_logic;
		in0, in1 : in  std_logic_vector(31 downto 0);
		o        : out std_logic_vector(31 downto 0));
end mux2_32;

architecture Behavioral of mux2_32 is
begin
	process(s, in0, in1)
	begin
		case s is
			when '0' => o <= in0 after 1 ns;
			when '1' => o <= in1 after 1 ns;
			when others => o <= in0;
		end case;
	end process;
end Behavioral;

