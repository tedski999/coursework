library ieee;
use ieee.std_logic_1164.all;

entity program_counter_32bit_tb is
end program_counter_32bit_tb;

architecture Behavior of program_counter_32bit_tb is

	component program_counter_32bit is
		port(
			clock, reset     : in std_logic;
			load_enable      : in std_logic;
			increment_enable : in std_logic;
			input            : in std_logic_vector(31 downto 0);
			output           : out std_logic_vector(31 downto 0));
	end component;

	signal clock              : std_logic := '0';
	signal reset, load_enable : std_logic;
	signal increment_enable   : std_logic;
	signal input, output      : std_logic_vector(31 downto 0);

begin

	-- Simulate a 1 MHz clock
	clock <= not clock after 500 ns;

	uut: program_counter_32bit port map(
		clock => clock, reset => reset,
		load_enable => load_enable,
		increment_enable => increment_enable,
		input => input,
		output => output);

	stim_proc: process
	begin

		-- Test #1 - Reset signal
		reset <= '1';
		wait until falling_edge(clock);
		assert output = x"00000000" report "program_counter_32bit test #1" severity failure;
		reset <= '0';

		-- Test #2 - Do nothing
		load_enable <= '0';
		increment_enable <= '0';
		input <= x"01010101";
		wait until falling_edge(clock);
		assert output = x"00000000" report "program_counter_32bit test #2" severity failure;

		-- Test #3 - Increment
		load_enable <= '0';
		increment_enable <= '1';
		input <= x"01010101";
		wait until falling_edge(clock);
		assert output = x"00000001" report "program_counter_32bit test #3" severity failure;

		-- Test #4 - Load
		load_enable <= '1';
		increment_enable <= '0';
		input <= x"00000005";
		wait until falling_edge(clock);
		assert output = x"00000006" report "program_counter_32bit test #4" severity failure;

		-- Test #5 - Load & Increment
		load_enable <= '1';
		increment_enable <= '1';
		input <= x"00000010";
		wait until falling_edge(clock);
		assert output = x"00000017" report "program_counter_32bit test #5" severity failure;

		std.env.finish;

	end process;
end;

