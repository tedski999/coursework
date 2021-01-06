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
	
		wait until falling_edge(clock);

		-- Test #1 - Write 0x00000001
		load <= '1';
		input <= x"00000001";
		wait until falling_edge(clock);
		assert output = x"00000001" report "reg_32bit test #1" severity failure;

		-- Test #2 - Don't write
		load <= '0';
		input <= x"00000002";
		wait until falling_edge(clock);
		assert output = x"00000001" report "reg_32bit test #2" severity failure;

		-- Test #3 - Overwrite with 0x00000003
		load <= '1';
		input <= x"00000003";
		wait until falling_edge(clock);
		assert output = x"00000003" report "reg_32bit test #3" severity failure;

		std.env.finish;

	end process;
end;

