library ieee;
use ieee.std_logic_1164.all;

entity reg_32bit_tb is
end reg_32bit_tb;

architecture Behavior of reg_32bit_tb is
	component reg_32bit
		port(
			clock, load : in std_logic;
			input       : in std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	signal clock  : std_logic := '0';
	signal load   : std_logic;
	signal input  : std_logic_vector(31 downto 0);
	signal output : std_logic_vector(31 downto 0);

begin

	-- Simulate a 1 MHz clock
	clock <= not clock after 500 ns;

	uut: reg_32bit port map(clock => clock, load => load, input => input, output => output);
	stim_proc: process
	begin
	
		-- load is low for this cycle, so the value shouldn't be loaded
		load <= '0';
		input <= x"0f0f0f0f";
		wait until falling_edge(clock);

		-- load is high for this cycle, so the value is loaded
		load <= '1';
		input <= x"12345678";
		wait until falling_edge(clock);

		-- load is low for this cycle, so the value shouldn't be loaded
		load <= '0';
		input <= x"87654321";
		wait until falling_edge(clock);
		
		-- Setting load high again will load the value on the next clock rising edge
		load <= '1';
		wait;

	end process;
end;

