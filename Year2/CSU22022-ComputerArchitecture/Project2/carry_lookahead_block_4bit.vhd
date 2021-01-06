library ieee;
use ieee.std_logic_1164.all;

entity carry_lookahead_block_4bit is
	port(
		a_input, b_input : in std_logic_vector(3 downto 0);
		carry_in         : in std_logic;
		g, p             : out std_logic;
		sum              : out std_logic_vector(3 downto 0));
end carry_lookahead_block_4bit;

architecture Behavioral of carry_lookahead_block_4bit is

	component full_adder
		port(
			a, b, carry_in : in std_logic;
			sum, carry_out : out std_logic);
	end component;

	signal c0, c1, c2             : std_logic;
	signal or0, or1, or2, or3     : std_logic;
	signal and0, and1, and2, and3 : std_logic;
	signal and4, and5, and6, and7 : std_logic;

begin

	or0 <= a_input(0) or b_input(0) after 5 ns;
	or1 <= a_input(1) or b_input(1) after 5 ns;
	or2 <= a_input(2) or b_input(2) after 5 ns;
	or3 <= a_input(3) or b_input(3) after 5 ns;

	and0 <= a_input(0) and b_input(0) after 5 ns;
	and1 <= a_input(1) and b_input(1) after 5 ns;
	and2 <= a_input(2) and b_input(2) after 5 ns;
	and3 <= a_input(3) and b_input(3) after 5 ns;

	and4 <= a_input(0) and b_input(0) after 5 ns;
	and5 <= a_input(1) and b_input(1) after 5 ns;
	and6 <= a_input(2) and b_input(2) after 5 ns;
	and7 <= a_input(3) and b_input(3) after 5 ns;


	fa0: full_adder port map(a => a_input(0), b => b_input(0), carry_in => carry_in, sum => sum(0));
	fa1: full_adder port map(a => a_input(1), b => b_input(1), carry_in => c0, sum => sum(1));
	fa2: full_adder port map(a => a_input(2), b => b_input(2), carry_in => c1, sum => sum(2));
	fa3: full_adder port map(a => a_input(3), b => b_input(3), carry_in => c2, sum => sum(3));

	-- TODO: remaining 4-bit block. did not have enough time to finish

end Behavioral;


