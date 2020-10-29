library ieee;
use ieee.std_logic_1164.all;

entity mux2_32_tb is
end mux2_32_tb;

architecture Behavior of mux2_32_tb is
	component mux2_32
		port(
			s        : in  std_logic;
			in0, in1 : in  std_logic_vector(31 downto 0);
			o        : out std_logic_vector(31 downto 0));
	end component;

	signal s        : std_logic := '0';
	signal in0, in1 : std_logic_vector(31 downto 0) := (others => '0');
	signal o        : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: mux2_32 port map(s => s, in0 => in0, in1 => in1, o => o);
	stim_proc: process
	begin
		in0 <= x"00000000";
		in1 <= x"ffffffff";
		wait for 10 ns;
		s <= '0';
		wait for 10 ns;
		assert o = x"00000000";
		s <= '1';
		wait for 10 ns;
		assert o = x"ffffffff";
	end process;
end;

