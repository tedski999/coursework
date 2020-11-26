library ieee;
use ieee.std_logic_1164.all;

entity reg_32bit is
	port(
		clock, load : in std_logic;
		input       : in std_logic_vector(31 downto 0);
		output      : out std_logic_vector(31 downto 0));
end reg_32bit;

architecture Behavioral of reg_32bit is
begin
	process(clock)
	begin
		-- Only load input on the clock rising edge when load is high
		if rising_edge(clock) and load='1' then
			output <= input after 5 ns;
		end if;
	end process;
end Behavioral;

