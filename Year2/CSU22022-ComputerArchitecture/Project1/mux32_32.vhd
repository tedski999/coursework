library ieee;
use ieee.std_logic_1164.all;

entity mux32_32 is
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
end mux32_32;

architecture Behavioral of mux32_32 is
begin
	process(
		s,
		in00, in01, in02, in03,
		in04, in05, in06, in07,
		in08, in09, in0a, in0b,
		in0c, in0d, in0e, in0f,
		in10, in11, in12, in13,
		in14, in15, in16, in17,
		in18, in19, in1a, in1b,
		in1c, in1d, in1e, in1f)
	begin
		case s is
			when "00000" => o <= in00 after 1 ns;
			when "00001" => o <= in01 after 1 ns;
			when "00010" => o <= in02 after 1 ns;
			when "00011" => o <= in03 after 1 ns;
			when "00100" => o <= in04 after 1 ns;
			when "00101" => o <= in05 after 1 ns;
			when "00110" => o <= in06 after 1 ns;
			when "00111" => o <= in07 after 1 ns;
			when "01000" => o <= in08 after 1 ns;
			when "01001" => o <= in09 after 1 ns;
			when "01010" => o <= in0a after 1 ns;
			when "01011" => o <= in0b after 1 ns;
			when "01100" => o <= in0c after 1 ns;
			when "01101" => o <= in0d after 1 ns;
			when "01110" => o <= in0e after 1 ns;
			when "01111" => o <= in0f after 1 ns;
			when "10000" => o <= in10 after 1 ns;
			when "10001" => o <= in11 after 1 ns;
			when "10010" => o <= in12 after 1 ns;
			when "10011" => o <= in13 after 1 ns;
			when "10100" => o <= in14 after 1 ns;
			when "10101" => o <= in15 after 1 ns;
			when "10110" => o <= in16 after 1 ns;
			when "10111" => o <= in17 after 1 ns;
			when "11000" => o <= in18 after 1 ns;
			when "11001" => o <= in19 after 1 ns;
			when "11010" => o <= in1a after 1 ns;
			when "11011" => o <= in1b after 1 ns;
			when "11100" => o <= in1c after 1 ns;
			when "11101" => o <= in1d after 1 ns;
			when "11110" => o <= in1e after 1 ns;
			when "11111" => o <= in1f after 1 ns;
			when others => o <= in00 after 1 ns;
		end case;
	end process;
end Behavioral;

