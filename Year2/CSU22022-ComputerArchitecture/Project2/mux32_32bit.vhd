library ieee;
use ieee.std_logic_1164.all;

entity mux32_32bit is
	port(
		line_select                    : in std_logic_vector(4 downto 0);
		line00, line01, line02, line03 : in std_logic_vector(31 downto 0);
		line04, line05, line06, line07 : in std_logic_vector(31 downto 0);
		line08, line09, line0a, line0b : in std_logic_vector(31 downto 0);
		line0c, line0d, line0e, line0f : in std_logic_vector(31 downto 0);
		line10, line11, line12, line13 : in std_logic_vector(31 downto 0);
		line14, line15, line16, line17 : in std_logic_vector(31 downto 0);
		line18, line19, line1a, line1b : in std_logic_vector(31 downto 0);
		line1c, line1d, line1e, line1f : in std_logic_vector(31 downto 0);
		output                         : out std_logic_vector(31 downto 0));
end mux32_32bit;

architecture Behavioral of mux32_32bit is
begin

	process(
		line_select,
		line00, line01, line02, line03,
		line04, line05, line06, line07,
		line08, line09, line0a, line0b,
		line0c, line0d, line0e, line0f,
		line10, line11, line12, line13,
		line14, line15, line16, line17,
		line18, line19, line1a, line1b,
		line1c, line1d, line1e, line1f)
	begin

		-- Set the output to the approptiate input line
		case line_select is
			when "00000" => output <= line00 after 5 ns;
			when "00001" => output <= line01 after 5 ns;
			when "00010" => output <= line02 after 5 ns;
			when "00011" => output <= line03 after 5 ns;
			when "00100" => output <= line04 after 5 ns;
			when "00101" => output <= line05 after 5 ns;
			when "00110" => output <= line06 after 5 ns;
			when "00111" => output <= line07 after 5 ns;
			when "01000" => output <= line08 after 5 ns;
			when "01001" => output <= line09 after 5 ns;
			when "01010" => output <= line0a after 5 ns;
			when "01011" => output <= line0b after 5 ns;
			when "01100" => output <= line0c after 5 ns;
			when "01101" => output <= line0d after 5 ns;
			when "01110" => output <= line0e after 5 ns;
			when "01111" => output <= line0f after 5 ns;
			when "10000" => output <= line10 after 5 ns;
			when "10001" => output <= line11 after 5 ns;
			when "10010" => output <= line12 after 5 ns;
			when "10011" => output <= line13 after 5 ns;
			when "10100" => output <= line14 after 5 ns;
			when "10101" => output <= line15 after 5 ns;
			when "10110" => output <= line16 after 5 ns;
			when "10111" => output <= line17 after 5 ns;
			when "11000" => output <= line18 after 5 ns;
			when "11001" => output <= line19 after 5 ns;
			when "11010" => output <= line1a after 5 ns;
			when "11011" => output <= line1b after 5 ns;
			when "11100" => output <= line1c after 5 ns;
			when "11101" => output <= line1d after 5 ns;
			when "11110" => output <= line1e after 5 ns;
			when "11111" => output <= line1f after 5 ns;
			when others =>  output <= x"00000000" after 5 ns;
		end case;

	end process;

end Behavioral;

