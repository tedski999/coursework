library ieee;
use ieee.std_logic_1164.all;

entity reg_32bit_tb is
end reg_32bit_tb;

architecture Behavior of reg_32bit_tb is
	component reg_32bit
		port(
			load   : in  std_logic;
			input  : in  std_logic_vector(31 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	signal load : std_logic := '0';
	signal input, output : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: reg_32bit port map(load => load, input => input, output => output);
	stim_proc: process
	begin
		load <= '0';
		input <= x"00000000";
		wait for 10 ns;

		input <= x"12345678";
		wait for 10 ns;

		load <= '1';
		wait for 10 ns;

		load <= '0';
		wait for 10 ns;

		input <= x"87654321";
		wait for 10 ns;

		load <= '0';
		wait for 10 ns;

		input <= x"00000000";
		wait;
	end process;
end;

