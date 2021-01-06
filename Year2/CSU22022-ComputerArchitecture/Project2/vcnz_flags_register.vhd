library ieee;
use ieee.std_logic_1164.all;

entity vcnz_flags_register is
	port(
		clock, load_enable : in std_logic;
		vcnz, resets       : in std_logic_vector(3 downto 0);
		flags              : out std_logic_vector(3 downto 0));
end vcnz_flags_register;

architecture Behavioral of vcnz_flags_register is
begin
	process(clock)
		variable state : std_logic_vector(3 downto 0);
	begin
		-- Load input on rising edge if load_enable is high.
		-- Resets a bit if its corresponding resets input bit is high.
		if rising_edge(clock) then
			if load_enable = '1' then
				state := vcnz;
			end if;
			flags <= state and not resets after 10 ns;
		end if;
	end process;
end Behavioral;

