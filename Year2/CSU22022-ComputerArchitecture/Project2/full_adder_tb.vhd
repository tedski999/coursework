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

		-- Test #1 - 0 + 0 + 0 = s0 c0
		a <= '0'; b <= '0'; carry_in <= '0'; wait for 25 ns;
		assert sum = '0'       report "mux2_1bit test #1 - sum"   severity failure;
		assert carry_out = '0' report "mux2_1bit test #1 - carry" severity failure;

		-- Test #2.0 - 0 + 0 + 1 = s1 c0
		a <= '0'; b <= '0'; carry_in <= '1'; wait for 25 ns;
		assert sum = '1'       report "mux2_1bit test #2.0 - sum"   severity failure;
		assert carry_out = '0' report "mux2_1bit test #2.0 - carry" severity failure;

		-- Test #2.1 - 0 + 1 + 0 = s1 c0
		a <= '0'; b <= '1'; carry_in <= '0'; wait for 25 ns;
		assert sum = '1'       report "mux2_1bit test #2.1 - sum"   severity failure;
		assert carry_out = '0' report "mux2_1bit test #2.1 - carry" severity failure;

		-- Test #2.2 - 1 + 0 + 0 = s1 c0
		a <= '1'; b <= '0'; carry_in <= '0'; wait for 25 ns;
		assert sum = '1'       report "mux2_1bit test #2.2 - sum"   severity failure;
		assert carry_out = '0' report "mux2_1bit test #2.2 - carry" severity failure;

		-- Test #3.0 - 1 + 1 + 0 = s0 c1
		a <= '1'; b <= '1'; carry_in <= '0'; wait for 25 ns; -- 1 + 1 + 0 = s0 c1
		assert sum = '0'       report "mux2_1bit test #3.0 - sum"   severity failure;
		assert carry_out = '1' report "mux2_1bit test #3.0 - carry" severity failure;

		-- Test #3.1 - 0 + 1 + 1 = s0 c1
		a <= '0'; b <= '1'; carry_in <= '1'; wait for 25 ns; -- 0 + 1 + 1 = s0 c1
		assert sum = '0'       report "mux2_1bit test #3.1 - sum"   severity failure;
		assert carry_out = '1' report "mux2_1bit test #3.1 - carry" severity failure;

		-- Test #3.2 - 1 + 0 + 1 = s0 c1
		a <= '1'; b <= '0'; carry_in <= '1'; wait for 25 ns; -- 1 + 0 + 1 = s0 c1
		assert sum = '0'       report "mux2_1bit test #3.2 - sum"   severity failure;
		assert carry_out = '1' report "mux2_1bit test #3.2 - carry" severity failure;

		-- Test #4 - 1 + 1 + 1 = s1 c1
		a <= '1'; b <= '1'; carry_in <= '1'; wait for 25 ns;
		assert sum = '1'       report "mux2_1bit test #4 - sum"   severity failure;
		assert carry_out = '1' report "mux2_1bit test #4 - carry" severity failure;
		
		std.env.finish;

	end process;
end;

