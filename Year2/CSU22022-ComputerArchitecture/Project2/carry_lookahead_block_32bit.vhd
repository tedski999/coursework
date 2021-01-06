library ieee;
use ieee.std_logic_1164.all;

entity carry_lookahead_block_32bit is
	port(
		a_input, b_input : in std_logic_vector(31 downto 0);
		carry_in         : in std_logic;
		sum              : out std_logic_vector(31 downto 0);
		carry_out        : out std_logic);
end carry_lookahead_block_32bit;

architecture Behavioral of carry_lookahead_block_32bit is

	component carry_lookahead_block_16bit
		port(
			a_input, b_input : in std_logic_vector(15 downto 0);
			carry_in         : in std_logic;
			g, p             : out std_logic;
			sum              : out std_logic_vector(15 downto 0));
	end component;

	signal carry_in_block_2 : std_logic;
	signal g0, g1, p0, p1 : std_logic;

begin

	carry_in_block_2 <= g0 or (p0 and carry_in) after 10 ns;
	carry_out <= (p1 and g0) or (p1 and carry_in and p0) after 10 ns;

	block0: carry_lookahead_block_16bit port map(a_input => a_input(15 downto 0), b_input => b_input(15 downto 0), carry_in => carry_in, g => g0, p => p0, sum => sum(15 downto 0));
	block1: carry_lookahead_block_16bit port map(a_input => a_input(31 downto 16), b_input => b_input(31 downto 16), carry_in => carry_in, g => g1, p => p1, sum => sum(31 downto 16));

end Behavioral;

