library ieee;
use ieee.std_logic_1164.all;

entity reg32 is
	port(
		load, clk : in  std_logic;
		d         : in  std_logic_vector(31 downto 0);
		o         : out std_logic_vector(31 downto 0));
end reg32;

architecture Behavioral of reg32 is
begin
	process(clk)
	begin
		if (rising_edge(clk)) then
			if load='1' then
				o <= d after 5 ns;
			end if;
		end if;
	end process;
end Behavioral;

