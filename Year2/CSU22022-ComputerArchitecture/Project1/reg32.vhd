library ieee;
use ieee.std_logic_1164.all;

entity reg32 is
	port(
		load   : in  std_logic;
		input  : in  std_logic_vector(31 downto 0);
		output : out std_logic_vector(31 downto 0));
end reg32;

architecture Behavioral of reg32 is
begin
	process(load, input)
	begin
		if load = '1' then
			output <= input after 1 ns;
		end if;
	end process;
end Behavioral;

