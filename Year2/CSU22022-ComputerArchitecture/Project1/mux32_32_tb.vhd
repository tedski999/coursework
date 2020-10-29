library ieee;
use ieee.std_logic_1164.all;

entity mux32_32_tb is
end mux32_32_tb;

architecture Behavior of mux32_32_tb is
	component mux32_32
		port(
			s : in std_logic_vector(4 downto 0);
			in00, in01, in02, in03 : in std_logic_vector(31 downto 0);
			in04, in05, in06, in07 : in std_logic_vector(31 downto 0);
			in08, in09, in0a, in0b : in std_logic_vector(31 downto 0);
			in0c, in0d, in0e, in0f : in std_logic_vector(31 downto 0);
			in10, in11, in12, in13 : in std_logic_vector(31 downto 0);
			in14, in15, in16, in17 : in std_logic_vector(31 downto 0);
			in18, in19, in1a, in1b : in std_logic_vector(31 downto 0);
			in1c, in1d, in1e, in1f : in std_logic_vector(31 downto 0);
			o : out std_logic_vector(31 downto 0));
	end component;

	signal s : std_logic_vector(4 downto 0) := (others => '0');
	signal in00, in01, in02, in03 : std_logic_vector(31 downto 0) := (others => '0');
	signal in04, in05, in06, in07 : std_logic_vector(31 downto 0) := (others => '0');
	signal in08, in09, in0a, in0b : std_logic_vector(31 downto 0) := (others => '0');
	signal in0c, in0d, in0e, in0f : std_logic_vector(31 downto 0) := (others => '0');
	signal in10, in11, in12, in13 : std_logic_vector(31 downto 0) := (others => '0');
	signal in14, in15, in16, in17 : std_logic_vector(31 downto 0) := (others => '0');
	signal in18, in19, in1a, in1b : std_logic_vector(31 downto 0) := (others => '0');
	signal in1c, in1d, in1e, in1f : std_logic_vector(31 downto 0) := (others => '0');
	signal o : std_logic_vector(31 downto 0);

begin
	uut: mux32_32 port map(
		s => s,
		in00 => in00, in01 => in01, in02 => in02, in03 => in03,
		in04 => in04, in05 => in05, in06 => in06, in07 => in07,
		in08 => in08, in09 => in09, in0a => in0a, in0b => in0b,
		in0c => in0c, in0d => in0d, in0e => in0e, in0f => in0f,
		in10 => in10, in11 => in11, in12 => in12, in13 => in13,
		in14 => in14, in15 => in15, in16 => in16, in17 => in17,
		in18 => in18, in19 => in19, in1a => in1a, in1b => in1b,
		in1c => in1c, in1d => in1d, in1e => in1e, in1f => in1f,
		o => o);
	stim_proc: process
	begin
		in00 <= x"00000000";
		in01 <= x"00000001";
		in02 <= x"00000002";
		in03 <= x"00000003";
		in04 <= x"00000004";
		in05 <= x"00000005";
		in06 <= x"00000006";
		in07 <= x"00000007";
		in08 <= x"00000008";
		in09 <= x"00000009";
		in0a <= x"0000000a";
		in0b <= x"0000000b";
		in0c <= x"0000000c";
		in0d <= x"0000000d";
		in0e <= x"0000000e";
		in0f <= x"0000000f";
		in10 <= x"00000010";
		in11 <= x"00000011";
		in12 <= x"00000012";
		in13 <= x"00000013";
		in14 <= x"00000014";
		in15 <= x"00000015";
		in16 <= x"00000016";
		in17 <= x"00000017";
		in18 <= x"00000018";
		in19 <= x"00000019";
		in1a <= x"0000001a";
		in1b <= x"0000001b";
		in1c <= x"0000001c";
		in1d <= x"0000001d";
		in1e <= x"0000001e";
		in1f <= x"0000001f";

		wait for 10 ns;
		s <= "00000";
		wait for 10 ns;
		assert o = x"00000001";
		s <= "00001";
		wait for 10 ns;
		assert o = x"00000002";
		s <= "00010";
		wait for 10 ns;
		assert o = x"00000003";
		s <= "00011";
		wait for 10 ns;
		assert o = x"00000004";
		s <= "00100";
		wait for 10 ns;
		assert o = x"00000005";
		s <= "00101";
		wait for 10 ns;
		assert o = x"00000006";
		s <= "00110";
		wait for 10 ns;
		assert o = x"00000007";
		s <= "00111";
		wait for 10 ns;
		assert o = x"00000008";
		s <= "01000";
		wait for 10 ns;
		assert o = x"00000009";
		s <= "01001";
		wait for 10 ns;
		assert o = x"0000000a";
		s <= "01010";
		wait for 10 ns;
		assert o = x"0000000b";
		s <= "01011";
		wait for 10 ns;
		assert o = x"0000000c";
		s <= "01100";
		wait for 10 ns;
		assert o = x"0000000d";
		s <= "01101";
		wait for 10 ns;
		assert o = x"0000000e";
		s <= "01110";
		wait for 10 ns;
		assert o = x"0000000f";
		s <= "01111";
		wait for 10 ns;
		assert o = x"00000010";
		s <= "10000";
		wait for 10 ns;
		assert o = x"00000011";
		s <= "10001";
		wait for 10 ns;
		assert o = x"00000012";
		s <= "10010";
		wait for 10 ns;
		assert o = x"00000013";
		s <= "10011";
		wait for 10 ns;
		assert o = x"00000014";
		s <= "10100";
		wait for 10 ns;
		assert o = x"00000015";
		s <= "10101";
		wait for 10 ns;
		assert o = x"00000016";
		s <= "10110";
		wait for 10 ns;
		assert o = x"00000017";
		s <= "10111";
		wait for 10 ns;
		assert o = x"00000018";
		s <= "11000";
		wait for 10 ns;
		assert o = x"00000019";
		s <= "11001";
		wait for 10 ns;
		assert o = x"0000001a";
		s <= "11010";
		wait for 10 ns;
		assert o = x"0000001b";
		s <= "11011";
		wait for 10 ns;
		assert o = x"0000001c";
		s <= "11100";
		wait for 10 ns;
		assert o = x"0000001d";
		s <= "11101";
		wait for 10 ns;
		assert o = x"0000001e";
		s <= "11110";
		wait for 10 ns;
		assert o = x"0000001f";
		s <= "11111";
		wait for 10 ns;
		assert o = x"0000001f";
	end process;
end;

