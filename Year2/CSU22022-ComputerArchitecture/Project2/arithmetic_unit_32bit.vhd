library ieee;
use ieee.std_logic_1164.all;

entity arithmetic_unit_32bit is
	port(
		operation        : in std_logic_vector(2 downto 0);
		a_input, b_input : in std_logic_vector(31 downto 0);
		output           : out std_logic_vector(31 downto 0);
		carry, overflow  : out std_logic);
end arithmetic_unit_32bit;

architecture Behavioral of arithmetic_unit_32bit is

	component full_adder
		port(
			a, b, carry_in : in std_logic;
			sum, carry_out : out std_logic);
	end component;

	component mux2_1bit
		port(
			line_select : in std_logic;
			lines       : in std_logic_vector(1 downto 0);
			output      : out std_logic);
	end component;

	signal b_input_inverse, y_input, ripple_carry_lines, result : std_logic_vector(31 downto 0);

begin

	-- The b_input logic multiplexers expect the operation(2 downto 1) into lines input to be in reverse.
	-- It's easier to just invert the line_select input.
	b_input_inverse <= not b_input after 5 ns;

	-- The overflow flag is set if the two inputs have the same sign, but the result has a different sign
	overflow <= not (a_input(31) xor b_input(31)) and (a_input(31) xor result(31)) after 15 ns;
	output <= result;

	-- Parallel adder (RCA) - Maximum gate delay of 480 ns (15 ns * 32)
	adder00: full_adder port map(a => a_input(0),  b => y_input(0),  carry_in => operation(0),           sum => result(0),  carry_out => ripple_carry_lines(0));
	adder01: full_adder port map(a => a_input(1),  b => y_input(1),  carry_in => ripple_carry_lines(0),  sum => result(1),  carry_out => ripple_carry_lines(1));
	adder02: full_adder port map(a => a_input(2),  b => y_input(2),  carry_in => ripple_carry_lines(1),  sum => result(2),  carry_out => ripple_carry_lines(2));
	adder03: full_adder port map(a => a_input(3),  b => y_input(3),  carry_in => ripple_carry_lines(2),  sum => result(3),  carry_out => ripple_carry_lines(3));
	adder04: full_adder port map(a => a_input(4),  b => y_input(4),  carry_in => ripple_carry_lines(3),  sum => result(4),  carry_out => ripple_carry_lines(4));
	adder05: full_adder port map(a => a_input(5),  b => y_input(5),  carry_in => ripple_carry_lines(4),  sum => result(5),  carry_out => ripple_carry_lines(5));
	adder06: full_adder port map(a => a_input(6),  b => y_input(6),  carry_in => ripple_carry_lines(5),  sum => result(6),  carry_out => ripple_carry_lines(6));
	adder07: full_adder port map(a => a_input(7),  b => y_input(7),  carry_in => ripple_carry_lines(6),  sum => result(7),  carry_out => ripple_carry_lines(7));
	adder08: full_adder port map(a => a_input(8),  b => y_input(8),  carry_in => ripple_carry_lines(7),  sum => result(8),  carry_out => ripple_carry_lines(8));
	adder09: full_adder port map(a => a_input(9),  b => y_input(9),  carry_in => ripple_carry_lines(8),  sum => result(9),  carry_out => ripple_carry_lines(9));
	adder0a: full_adder port map(a => a_input(10), b => y_input(10), carry_in => ripple_carry_lines(9),  sum => result(10), carry_out => ripple_carry_lines(10));
	adder0b: full_adder port map(a => a_input(11), b => y_input(11), carry_in => ripple_carry_lines(10), sum => result(11), carry_out => ripple_carry_lines(11));
	adder0c: full_adder port map(a => a_input(12), b => y_input(12), carry_in => ripple_carry_lines(11), sum => result(12), carry_out => ripple_carry_lines(12));
	adder0d: full_adder port map(a => a_input(13), b => y_input(13), carry_in => ripple_carry_lines(12), sum => result(13), carry_out => ripple_carry_lines(13));
	adder0e: full_adder port map(a => a_input(14), b => y_input(14), carry_in => ripple_carry_lines(13), sum => result(14), carry_out => ripple_carry_lines(14));
	adder0f: full_adder port map(a => a_input(15), b => y_input(15), carry_in => ripple_carry_lines(14), sum => result(15), carry_out => ripple_carry_lines(15));
	adder10: full_adder port map(a => a_input(16), b => y_input(16), carry_in => ripple_carry_lines(15), sum => result(16), carry_out => ripple_carry_lines(16));
	adder11: full_adder port map(a => a_input(17), b => y_input(17), carry_in => ripple_carry_lines(16), sum => result(17), carry_out => ripple_carry_lines(17));
	adder12: full_adder port map(a => a_input(18), b => y_input(18), carry_in => ripple_carry_lines(17), sum => result(18), carry_out => ripple_carry_lines(18));
	adder13: full_adder port map(a => a_input(19), b => y_input(19), carry_in => ripple_carry_lines(18), sum => result(19), carry_out => ripple_carry_lines(19));
	adder14: full_adder port map(a => a_input(20), b => y_input(20), carry_in => ripple_carry_lines(19), sum => result(20), carry_out => ripple_carry_lines(20));
	adder15: full_adder port map(a => a_input(21), b => y_input(21), carry_in => ripple_carry_lines(20), sum => result(21), carry_out => ripple_carry_lines(21));
	adder16: full_adder port map(a => a_input(22), b => y_input(22), carry_in => ripple_carry_lines(21), sum => result(22), carry_out => ripple_carry_lines(22));
	adder17: full_adder port map(a => a_input(23), b => y_input(23), carry_in => ripple_carry_lines(22), sum => result(23), carry_out => ripple_carry_lines(23));
	adder18: full_adder port map(a => a_input(24), b => y_input(24), carry_in => ripple_carry_lines(23), sum => result(24), carry_out => ripple_carry_lines(24));
	adder19: full_adder port map(a => a_input(25), b => y_input(25), carry_in => ripple_carry_lines(24), sum => result(25), carry_out => ripple_carry_lines(25));
	adder1a: full_adder port map(a => a_input(26), b => y_input(26), carry_in => ripple_carry_lines(25), sum => result(26), carry_out => ripple_carry_lines(26));
	adder1b: full_adder port map(a => a_input(27), b => y_input(27), carry_in => ripple_carry_lines(26), sum => result(27), carry_out => ripple_carry_lines(27));
	adder1c: full_adder port map(a => a_input(28), b => y_input(28), carry_in => ripple_carry_lines(27), sum => result(28), carry_out => ripple_carry_lines(28));
	adder1d: full_adder port map(a => a_input(29), b => y_input(29), carry_in => ripple_carry_lines(28), sum => result(29), carry_out => ripple_carry_lines(29));
	adder1e: full_adder port map(a => a_input(30), b => y_input(30), carry_in => ripple_carry_lines(29), sum => result(30), carry_out => ripple_carry_lines(30));
	adder1f: full_adder port map(a => a_input(31), b => y_input(31), carry_in => ripple_carry_lines(30), sum => result(31), carry_out => carry);

	-- b_input logic
	b_input_mux00: mux2_1bit port map(line_select => b_input_inverse(0),  lines => operation(2 downto 1), output => y_input(0));
	b_input_mux01: mux2_1bit port map(line_select => b_input_inverse(1),  lines => operation(2 downto 1), output => y_input(1));
	b_input_mux02: mux2_1bit port map(line_select => b_input_inverse(2),  lines => operation(2 downto 1), output => y_input(2));
	b_input_mux03: mux2_1bit port map(line_select => b_input_inverse(3),  lines => operation(2 downto 1), output => y_input(3));
	b_input_mux04: mux2_1bit port map(line_select => b_input_inverse(4),  lines => operation(2 downto 1), output => y_input(4));
	b_input_mux05: mux2_1bit port map(line_select => b_input_inverse(5),  lines => operation(2 downto 1), output => y_input(5));
	b_input_mux06: mux2_1bit port map(line_select => b_input_inverse(6),  lines => operation(2 downto 1), output => y_input(6));
	b_input_mux07: mux2_1bit port map(line_select => b_input_inverse(7),  lines => operation(2 downto 1), output => y_input(7));
	b_input_mux08: mux2_1bit port map(line_select => b_input_inverse(8),  lines => operation(2 downto 1), output => y_input(8));
	b_input_mux09: mux2_1bit port map(line_select => b_input_inverse(9),  lines => operation(2 downto 1), output => y_input(9));
	b_input_mux0a: mux2_1bit port map(line_select => b_input_inverse(10), lines => operation(2 downto 1), output => y_input(10));
	b_input_mux0b: mux2_1bit port map(line_select => b_input_inverse(11), lines => operation(2 downto 1), output => y_input(11));
	b_input_mux0c: mux2_1bit port map(line_select => b_input_inverse(12), lines => operation(2 downto 1), output => y_input(12));
	b_input_mux0d: mux2_1bit port map(line_select => b_input_inverse(13), lines => operation(2 downto 1), output => y_input(13));
	b_input_mux0e: mux2_1bit port map(line_select => b_input_inverse(14), lines => operation(2 downto 1), output => y_input(14));
	b_input_mux0f: mux2_1bit port map(line_select => b_input_inverse(15), lines => operation(2 downto 1), output => y_input(15));
	b_input_mux10: mux2_1bit port map(line_select => b_input_inverse(16), lines => operation(2 downto 1), output => y_input(16));
	b_input_mux11: mux2_1bit port map(line_select => b_input_inverse(17), lines => operation(2 downto 1), output => y_input(17));
	b_input_mux12: mux2_1bit port map(line_select => b_input_inverse(18), lines => operation(2 downto 1), output => y_input(18));
	b_input_mux13: mux2_1bit port map(line_select => b_input_inverse(19), lines => operation(2 downto 1), output => y_input(19));
	b_input_mux14: mux2_1bit port map(line_select => b_input_inverse(20), lines => operation(2 downto 1), output => y_input(20));
	b_input_mux15: mux2_1bit port map(line_select => b_input_inverse(21), lines => operation(2 downto 1), output => y_input(21));
	b_input_mux16: mux2_1bit port map(line_select => b_input_inverse(22), lines => operation(2 downto 1), output => y_input(22));
	b_input_mux17: mux2_1bit port map(line_select => b_input_inverse(23), lines => operation(2 downto 1), output => y_input(23));
	b_input_mux18: mux2_1bit port map(line_select => b_input_inverse(24), lines => operation(2 downto 1), output => y_input(24));
	b_input_mux19: mux2_1bit port map(line_select => b_input_inverse(25), lines => operation(2 downto 1), output => y_input(25));
	b_input_mux1a: mux2_1bit port map(line_select => b_input_inverse(26), lines => operation(2 downto 1), output => y_input(26));
	b_input_mux1b: mux2_1bit port map(line_select => b_input_inverse(27), lines => operation(2 downto 1), output => y_input(27));
	b_input_mux1c: mux2_1bit port map(line_select => b_input_inverse(28), lines => operation(2 downto 1), output => y_input(28));
	b_input_mux1d: mux2_1bit port map(line_select => b_input_inverse(29), lines => operation(2 downto 1), output => y_input(29));
	b_input_mux1e: mux2_1bit port map(line_select => b_input_inverse(30), lines => operation(2 downto 1), output => y_input(30));
	b_input_mux1f: mux2_1bit port map(line_select => b_input_inverse(31), lines => operation(2 downto 1), output => y_input(31));

end Behavioral;

