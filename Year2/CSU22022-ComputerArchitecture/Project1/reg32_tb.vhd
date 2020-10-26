library ieee;
use ieee.std_logic_1164.all;

entity reg32_tb is
end reg32_tb;

architecture Behavior of reg32_tb is
	component reg32
		port(
			load, clk : in  std_logic;
			d         : in  std_logic_vector(31 downto 0);
			o         : out std_logic_vector(31 downto 0));
	end component;

	signal load, clk : std_logic := '0';
	signal d, o      : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: reg32 port map(load => load, clk => clk, d => d, o => o);
	
	process
	begin
		clk <= not clk after 10 ns;
		wait for 10 ns;
	end process;
	
	stim_proc: process
	begin
		wait for 10 ns;
		d <= x"12345678";
		wait for 10 ns;
		load <= '1';
		wait for 20 ns;
		assert o = x"12345678";
		wait for 20 ns;
		load <= '0';
		wait for 10 ns;
		d <= x"87654321";
		wait for 10 ns;
		assert o = x"12345678";
		load <= '1';
		wait for 10 ns;
		assert o = x"87654321";
	end process;
end;

