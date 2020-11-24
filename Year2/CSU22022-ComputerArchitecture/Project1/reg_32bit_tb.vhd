library ieee;
use ieee.std_logic_1164.all;

entity reg_32bit_tb is
end reg_32bit_tb;

architecture Behavior of reg_32bit_tb is
	component reg_32bit
		port(
			clk, load   : in  std_logic;
			input       : in  std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	signal clk : std_logic := '0';
	signal load : std_logic;
	signal input, output : std_logic_vector(31 downto 0);

begin

	-- Simulate a 10 MHz clock
	clk <= not clk after 50 ns;

	uut: reg_32bit port map(clk => clk, load => load, input => input, output => output);
	stim_proc: process
	begin
	
		-- load is low for this cycle, so the value shouldn't be loaded
		load <= '0';
		input <= x"0f0f0f0f";
		wait for 100 ns;

		-- load is high for this cycle, so the value is loaded
		load <= '1';
		input <= x"12345678";
		wait for 100 ns;

		-- load is low for this cycle, so the value shouldn't be loaded
		load <= '0';
		input <= x"87654321";
		wait for 100 ns;
		
		-- Setting load high again will load the value on the next clock rising edge
		load <= '1';
		
		wait;
	end process;
end;

