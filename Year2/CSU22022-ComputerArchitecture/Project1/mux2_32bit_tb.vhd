library ieee;
use ieee.std_logic_1164.all;

entity mux2_32bit_tb is
end mux2_32bit_tb;

architecture Behavior of mux2_32bit_tb is
	component mux2_32bit
		port(
			line_select  : in  std_logic;
			line0, line1 : in  std_logic_vector(31 downto 0);
			output       : out std_logic_vector(31 downto 0));
	end component;

	signal line_select  : std_logic := '0';
	signal line0, line1 : std_logic_vector(31 downto 0) := (others => '0');
	signal output       : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: mux2_32bit port map(line_select => line_select, line0 => line0, line1 => line1, output => output);
	stim_proc: process
	begin
		line_select <= '0';
		line0 <= x"00000000";
		line1 <= x"ffffffff";
		wait for 10 ns;

		line_select <= '1';
		wait for 10 ns;

		line_select <= '0';
		wait for 10 ns;

		line0 <= x"f0f0f0f0";
		wait for 10 ns;

		line1 <= x"0f0f0f0f";
		wait;
	end process;
end;

