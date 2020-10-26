library ieee;
use ieee.std_logic_1164.all;

entity mux32_32_tb is
end mux32_32_tb;

architecture Behavior of mux32_32_tb is
	component mux32_32
		port(
			s : in  std_logic_vector(4 downto 0);
			in0,  in1,  in2,  in3  : in  std_logic_vector(31 downto 0);
			in4,  in5,  in6,  in7  : in  std_logic_vector(31 downto 0);
			in8,  in9,  in10, in11 : in  std_logic_vector(31 downto 0);
			in12, in13, in14, in15 : in  std_logic_vector(31 downto 0);
			in16, in17, in18, in19 : in  std_logic_vector(31 downto 0);
			in20, in21, in22, in23 : in  std_logic_vector(31 downto 0);
			in24, in25, in26, in27 : in  std_logic_vector(31 downto 0);
			in28, in29, in30, in31 : in  std_logic_vector(31 downto 0);
			z : out std_logic_vector(31 downto 0));
	end component;

	signal s : std_logic_vector(4 downto 0) := (others => '0');
	signal in0,  in1,  in2,  in3  : std_logic_vector(31 downto 0) := (others => '0');
	signal in4,  in5,  in6,  in7  : std_logic_vector(31 downto 0) := (others => '0');
	signal in8,  in9,  in10, in11 : std_logic_vector(31 downto 0) := (others => '0');
	signal in12, in13, in14, in15 : std_logic_vector(31 downto 0) := (others => '0');
	signal in16, in17, in18, in19 : std_logic_vector(31 downto 0) := (others => '0');
	signal in20, in21, in22, in23 : std_logic_vector(31 downto 0) := (others => '0');
	signal in24, in25, in26, in27 : std_logic_vector(31 downto 0) := (others => '0');
	signal in28, in29, in30, in31 : std_logic_vector(31 downto 0) := (others => '0');
	signal z : std_logic_vector(31 downto 0);

begin
	uut: mux32_32 port map(
		s => s,
		in0 => in0,   in1 => in1,   in2 => in2,   in3 => in3,
		in4 => in4,   in5 => in5,   in6 => in6,   in7 => in7,
		in8 => in8,   in9 => in9,   in10 => in10, in11 => in11,
		in12 => in12, in13 => in13, in14 => in14, in15 => in15,
		in16 => in16, in17 => in17, in18 => in18, in19 => in19,
		in20 => in20, in21 => in21, in22 => in22, in23 => in23,
		in24 => in24, in25 => in25, in26 => in26, in27 => in27,
		in28 => in28, in29 => in29, in30 => in30, in31 => in31,
		z => z);
	stim_proc: process
	begin
		in0 <= x"00000000";
		in1 <= x"00000001";
		in2 <= x"00000002";
		in3 <= x"00000003";
		in4 <= x"00000004";
		in5 <= x"00000005";
		in6 <= x"00000006";
		in7 <= x"00000007";
		in8 <= x"00000008";
		in9 <= x"00000009";
		in10 <= x"0000000a";
		in11 <= x"0000000b";
		in12 <= x"0000000c";
		in13 <= x"0000000d";
		in14 <= x"0000000e";
		in15 <= x"0000000f";
		in16 <= x"00000010";
		in17 <= x"00000011";
		in18 <= x"00000012";
		in19 <= x"00000013";
		in20 <= x"00000014";
		in21 <= x"00000015";
		in22 <= x"00000016";
		in23 <= x"00000017";
		in24 <= x"00000018";
		in25 <= x"00000019";
		in26 <= x"0000001a";
		in27 <= x"0000001b";
		in28 <= x"0000001c";
		in29 <= x"0000001d";
		in30 <= x"0000001e";
		in31 <= x"0000001f";

		wait for 10 ns;
		s <= "00000";
		wait for 10 ns;
		assert z = x"00000001";
		s <= "00001";
		wait for 10 ns;
		assert z = x"00000002";
		s <= "00010";
		wait for 10 ns;
		assert z = x"00000003";
		s <= "00011";
		wait for 10 ns;
		assert z = x"00000004";
		s <= "00100";
		wait for 10 ns;
		assert z = x"00000005";
		s <= "00101";
		wait for 10 ns;
		assert z = x"00000006";
		s <= "00110";
		wait for 10 ns;
		assert z = x"00000007";
		s <= "00111";
		wait for 10 ns;
		assert z = x"00000008";
		s <= "01000";
		wait for 10 ns;
		assert z = x"00000009";
		s <= "01001";
		wait for 10 ns;
		assert z = x"0000000a";
		s <= "01010";
		wait for 10 ns;
		assert z = x"0000000b";
		s <= "01011";
		wait for 10 ns;
		assert z = x"0000000c";
		s <= "01100";
		wait for 10 ns;
		assert z = x"0000000d";
		s <= "01101";
		wait for 10 ns;
		assert z = x"0000000e";
		s <= "01110";
		wait for 10 ns;
		assert z = x"0000000f";
		s <= "01111";
		wait for 10 ns;
		assert z = x"00000010";
		s <= "10000";
		wait for 10 ns;
		assert z = x"00000011";
		s <= "10001";
		wait for 10 ns;
		assert z = x"00000012";
		s <= "10010";
		wait for 10 ns;
		assert z = x"00000013";
		s <= "10011";
		wait for 10 ns;
		assert z = x"00000014";
		s <= "10100";
		wait for 10 ns;
		assert z = x"00000015";
		s <= "10101";
		wait for 10 ns;
		assert z = x"00000016";
		s <= "10110";
		wait for 10 ns;
		assert z = x"00000017";
		s <= "10111";
		wait for 10 ns;
		assert z = x"00000018";
		s <= "11000";
		wait for 10 ns;
		assert z = x"00000019";
		s <= "11001";
		wait for 10 ns;
		assert z = x"0000001a";
		s <= "11010";
		wait for 10 ns;
		assert z = x"0000001b";
		s <= "11011";
		wait for 10 ns;
		assert z = x"0000001c";
		s <= "11100";
		wait for 10 ns;
		assert z = x"0000001d";
		s <= "11101";
		wait for 10 ns;
		assert z = x"0000001e";
		s <= "11110";
		wait for 10 ns;
		assert z = x"0000001f";
		s <= "11111";
		wait for 10 ns;
		assert z = x"0000001f";
	end process;
end;

