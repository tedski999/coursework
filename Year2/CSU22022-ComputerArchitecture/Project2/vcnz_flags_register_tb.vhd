
library ieee;
use ieee.std_logic_1164.all;

entity vcnz_flags_register_tb is
end vcnz_flags_register_tb;

architecture Behavior of vcnz_flags_register_tb is
	component vcnz_flags_register
		port(
			clock, load_enable : in std_logic;
			vcnz, resets       : in std_logic_vector(3 downto 0);
			flags              : out std_logic_vector(3 downto 0));
	end component;

	signal clock        : std_logic := '0';
	signal load_enable  : std_logic;
	signal vcnz, resets : std_logic_vector(3 downto 0);
	signal flags        : std_logic_vector(3 downto 0);

begin

	-- Simulate a 1 MHz clock
	clock <= not clock after 500 ns;

	uut: vcnz_flags_register port map(
		clock => clock, load_enable => load_enable,
		vcnz => vcnz, resets => resets,
		flags => flags);
	stim_proc: process
	begin
	
		wait until falling_edge(clock);

		-- Test #1 - Write 0101
		load_enable <= '1';
		vcnz <= "0101";
		resets <= "0000";
		wait until falling_edge(clock);
		assert flags = "0101" report "vcnz_flags_register test #1" severity failure;

		-- Test #2 - Don't write
		load_enable <= '0';
		vcnz <= "1111";
		resets <= "0000";
		wait until falling_edge(clock);
		assert flags = "0101" report "vcnz_flags_register test #2" severity failure;

		-- Test #3 - Reset v flag
		load_enable <= '0';
		vcnz <= "0000";
		resets <= "0001";
		wait until falling_edge(clock);
		assert flags = "0100" report "vcnz_flags_register test #3" severity failure;

		-- Test #3 - Write 1110, reset v and c flags
		load_enable <= '1';
		vcnz <= "1110";
		resets <= "0011";
		wait until falling_edge(clock);
		assert flags = "1100" report "vcnz_flags_register test #4" severity failure;

		std.env.finish;

	end process;
end;

