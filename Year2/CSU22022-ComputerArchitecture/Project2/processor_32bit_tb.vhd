library ieee;
use ieee.std_logic_1164.all;

entity processor_32bit_tb is
end processor_32bit_tb;

architecture Behavior of processor_32bit_tb is
	component processor_32bit
		port(clock, reset : std_logic);
	end component;
	signal clock, reset : std_logic := '0';
begin

	-- Simulate 1 MHz clock
	clock <= not clock after 500 ns;

	uut: processor_32bit port map(clock => clock, reset => reset);
	stim_proc: process
	begin

		std.env.stop; -- Manual breakpoint
	
		-- Reset processor to begin program in memory
		reset <= '1';
		wait until rising_edge(clock);
		reset <= '0';

		wait;
		
	end process;
end;

