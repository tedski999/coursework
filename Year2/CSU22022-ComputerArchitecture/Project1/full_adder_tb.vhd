library ieee;
use ieee.std_logic_1164.all;

entity full_adder_tb is
end full_adder_tb;

architecture Behavior of full_adder_tb is
	component full_adder
		port(
			a, b, carry_in : in std_logic;
			sum, carry_out : out std_logic);
	end component;

	signal a, b, carry_in : std_logic;
	signal sum, carry_out : std_logic;

begin
	uut: full_adder port map(a => a, b => b, carry_in => carry_in, sum => sum, carry_out => carry_out);
	stim_proc: process
	begin

		-- Adding 0 bits: sum = 0, carry_out = 0
		a <= '0'; b <= '0'; carry_in <= '0'; wait for 20 ns; -- 0 + 0 + 0 = s0 c0

		-- Adding 1 bit: sum = 1, carry_out = 0
		a <= '0'; b <= '0'; carry_in <= '1'; wait for 20 ns; -- 0 + 0 + 1
		a <= '0'; b <= '1'; carry_in <= '0'; wait for 20 ns; -- 0 + 1 + 0
		a <= '1'; b <= '0'; carry_in <= '0'; wait for 20 ns; -- 1 + 0 + 0

		-- Adding 2 bits: sum = 0, carry_out = 1
		a <= '1'; b <= '1'; carry_in <= '0'; wait for 20 ns; -- 1 + 1 + 0
		a <= '0'; b <= '1'; carry_in <= '1'; wait for 20 ns; -- 0 + 1 + 1
		a <= '1'; b <= '0'; carry_in <= '1'; wait for 20 ns; -- 1 + 0 + 1

		-- Adding 3 bits: sum = 1, carry_out = 1
		a <= '1'; b <= '1'; carry_in <= '1'; wait; -- 1 + 1 + 1

	end process;
end;

