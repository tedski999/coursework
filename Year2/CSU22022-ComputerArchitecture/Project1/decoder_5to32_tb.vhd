library ieee;
use ieee.std_logic_1164.all;

entity decoder_5to32_tb is
end decoder_5to32_tb;

architecture Behavior of decoder_5to32_tb is
	component decoder_5to32
		port(
			s : in  std_logic_vector(4 downto 0);
			o : out std_logic_vector(31 downto 0));
	end component;

	signal s : std_logic_vector(4 downto 0) := (others => '0');
	signal o : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: decoder_5to32 port map(s => s, o => o);
	stim_proc: process
	begin
		s <= "00000";
		wait for 10 ns;
		assert o = "00000000000000000000000000000001";
		s <= "00001";
		wait for 10 ns;
		assert o = "00000000000000000000000000000010";
		s <= "00010";
		wait for 10 ns;
		assert o = "00000000000000000000000000000100";
		s <= "00011";
		wait for 10 ns;
		assert o = "00000000000000000000000000001000";
		s <= "00100";
		wait for 10 ns;
		assert o = "00000000000000000000000000010000";
		s <= "00101";
		wait for 10 ns;
		assert o = "00000000000000000000000000100000";
		s <= "00110";
		wait for 10 ns;
		assert o = "00000000000000000000000001000000";
		s <= "00111";
		wait for 10 ns;
		assert o = "00000000000000000000000010000000";
		s <= "01000";
		wait for 10 ns;
		assert o = "00000000000000000000000100000000";
		s <= "01001";
		wait for 10 ns;
		assert o = "00000000000000000000001000000000";
		s <= "01010";
		wait for 10 ns;
		assert o = "00000000000000000000010000000000";
		s <= "01011";
		wait for 10 ns;
		assert o = "00000000000000000000100000000000";
		s <= "01100";
		wait for 10 ns;
		assert o = "00000000000000000001000000000000";
		s <= "01101";
		wait for 10 ns;
		assert o = "00000000000000000010000000000000";
		s <= "01110";
		wait for 10 ns;
		assert o = "00000000000000000100000000000000";
		s <= "01111";
		wait for 10 ns;
		assert o = "00000000000000001000000000000000";
		s <= "10000";
		wait for 10 ns;
		assert o = "00000000000000010000000000000000";
		s <= "10001";
		wait for 10 ns;
		assert o = "00000000000000100000000000000000";
		s <= "10010";
		wait for 10 ns;
		assert o = "00000000000001000000000000000000";
		s <= "10011";
		wait for 10 ns;
		assert o = "00000000000010000000000000000000";
		s <= "10100";
		wait for 10 ns;
		assert o = "00000000000100000000000000000000";
		s <= "10101";
		wait for 10 ns;
		assert o = "00000000001000000000000000000000";
		s <= "10110";
		wait for 10 ns;
		assert o = "00000000010000000000000000000000";
		s <= "10111";
		wait for 10 ns;
		assert o = "00000000100000000000000000000000";
		s <= "11000";
		wait for 10 ns;
		assert o = "00000001000000000000000000000000";
		s <= "11001";
		wait for 10 ns;
		assert o = "00000010000000000000000000000000";
		s <= "11010";
		wait for 10 ns;
		assert o = "00000100000000000000000000000000";
		s <= "11011";
		wait for 10 ns;
		assert o = "00001000000000000000000000000000";
		s <= "11100";
		wait for 10 ns;
		assert o = "00010000000000000000000000000000";
		s <= "11101";
		wait for 10 ns;
		assert o = "00100000000000000000000000000000";
		s <= "11110";
		wait for 10 ns;
		assert o = "01000000000000000000000000000000";
		s <= "11111";
		wait for 10 ns;
		assert o = "10000000000000000000000000000000";
		
		wait for 1000 ns;
		
	end process;
end;

