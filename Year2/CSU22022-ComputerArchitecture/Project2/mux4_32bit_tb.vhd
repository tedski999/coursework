library ieee;
use ieee.std_logic_1164.all;

entity mux4_32bit_tb is
end mux4_32bit_tb;

architecture Behavior of mux4_32bit_tb is
	component mux4_32bit
		port(
			line_select  : in std_logic_vector(1 downto 0);
			line0, line1 : in std_logic_vector(31 downto 0);
			line2, line3 : in std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal line_select  : std_logic_vector(1 downto 0);
	signal line0, line1 : std_logic_vector(31 downto 0);
	signal line2, line3 : std_logic_vector(31 downto 0);
	signal output       : std_logic_vector(31 downto 0);

begin
	uut: mux4_32bit port map(
		line_select => line_select,
		line0 => line0, line1 => line1,
		line2 => line2, line3 => line3,
		output => output);
	stim_proc: process
	begin

		-- Test #1 - Switching selected line
		line0 <= x"00000000";
		line1 <= x"00000001";
		line2 <= x"00000002";
		line3 <= x"00000003";
		line_select <= "00"; wait for 25 ns; assert output = x"00000000" report "mux4_32bit test #1.0" severity failure;
		line_select <= "01"; wait for 25 ns; assert output = x"00000001" report "mux4_32bit test #1.1" severity failure;
		line_select <= "10"; wait for 25 ns; assert output = x"00000002" report "mux4_32bit test #1.2" severity failure;
		line_select <= "11"; wait for 25 ns; assert output = x"00000003" report "mux4_32bit test #1.3" severity failure;
	
		-- Test #2 - Changing lines input
		line_select <= "00";
		line0 <= x"00000000"; wait for 25 ns; assert output = x"00000000" report "mux4_32bit test #2.0" severity failure;
		line0 <= x"00000001"; wait for 25 ns; assert output = x"00000001" report "mux4_32bit test #2.1" severity failure;

		std.env.finish;
		
	end process;
end;

