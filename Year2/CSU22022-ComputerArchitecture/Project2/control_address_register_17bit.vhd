library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity control_address_register_17bit is
	port(
		clock, reset      : in std_logic;
		increment_disable : in std_logic;
		input_addr        : in std_logic_vector(16 downto 0);
		output            : out std_logic_vector(16 downto 0));
end control_address_register_17bit;

architecture Behavioral of control_address_register_17bit is
begin

	process(clock)
		variable new_addr : unsigned(16 downto 0);
	begin

		if rising_edge(clock) then
			if reset = '0' then
				new_addr := unsigned(input_addr);
				if increment_disable = '0' then
					new_addr := new_addr + 1;
				end if;
			else
				new_addr := "00000000000000001"; -- NOTE: control address on reset (Opcode IF)
			end if;

			output <= std_logic_vector(new_addr) after 5 ns;

		end if;

	end process;

end Behavioral;

