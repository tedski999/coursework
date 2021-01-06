library ieee;
use ieee.std_logic_1164.all;

entity memory32_32bit_tb is
end memory32_32bit_tb;

architecture Behavior of memory32_32bit_tb is
	component memory32_32bit is
		port(
			clock, write_enable : in std_logic;
			input_data, address : in std_logic_vector(31 downto 0);
			output              : out std_logic_vector(31 downto 0));
	end component;

	signal clock               : std_logic := '0';
	signal write_enable        : std_logic;
	signal input_data, address : std_logic_vector(31 downto 0);
	signal output              : std_logic_vector(31 downto 0);

begin

	-- Simulate 1 MHz clock
	clock <= not clock after 500 ns;

	uut: memory32_32bit port map(
		clock => clock, write_enable => write_enable,
		input_data => input_data, address => address,
		output => output);

	stim_proc: process
	begin

		wait until falling_edge(clock);

		-- Test #1 - Reading from 0x00
		write_enable <= '0';
		address <= x"00000000";
		input_data <= x"00000000";
		wait until falling_edge(clock);
		assert output = "00000000000000111000010000001110" report "memory32_32bit test #1" severity failure;

		-- Test #2 - Writing 0x00000001 to 0x25
		write_enable <= '1';
		address <= x"00000025";
		input_data <= x"00000001";
		wait until falling_edge(clock);
		assert output = x"00000001" report "memory32_32bit test #2" severity failure;

		-- Test #3 - Writing 0x00000002 to 0x26
		write_enable <= '1';
		address <= x"00000026";
		input_data <= x"00000002";
		wait until falling_edge(clock);
		assert output = x"00000002" report "memory32_32bit test #3" severity failure;

		-- Test #4 - Reading from 0x25
		write_enable <= '0';
		address <= x"00000025";
		input_data <= x"00000002";
		wait until falling_edge(clock);
		assert output = x"00000001" report "memory32_32bit test #4" severity failure;

		std.env.finish;

	end process;
end;

