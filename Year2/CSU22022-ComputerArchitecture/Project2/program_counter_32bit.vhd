library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity program_counter_32bit is
	port(
		clock, reset     : in std_logic;
		load_enable      : in std_logic;
		increment_enable : in std_logic;
		input            : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0));
end program_counter_32bit;

architecture Behavioral of program_counter_32bit is

	signal counter : std_logic_vector(31 downto 0);

begin

	output <= counter;

	process(clock)
		variable new_counter : unsigned(31 downto 0);
	begin

		if rising_edge(clock) then
			if reset = '0' then
				new_counter := unsigned(counter);

				if load_enable = '1' then
					new_counter := new_counter + unsigned(input);
				end if;

				if increment_enable = '1' then
					new_counter := new_counter + 1;
				end if;
			else
				new_counter := x"00000000"; -- NOTE: PC on reset
			end if;

			counter <= std_logic_vector(new_counter) after 5 ns;
		end if;
	end process;

end Behavioral;

