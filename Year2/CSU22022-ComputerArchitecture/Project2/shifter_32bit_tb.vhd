library ieee;
use ieee.std_logic_1164.all;

entity shifter_32bit_tb is
end shifter_32bit_tb;

architecture Behavior of shifter_32bit_tb is
	component shifter_32bit
		port(
			left, right : in std_logic;
			operation   : in std_logic_vector(1 downto 0);
			input       : in std_logic_vector(31 downto 0);
			output      : out std_logic_vector(31 downto 0));
	end component;

	signal left, right : std_logic;
	signal operation   : std_logic_vector(1 downto 0);
	signal input       : std_logic_vector(31 downto 0);
	signal output      : std_logic_vector(31 downto 0);

begin
	uut: shifter_32bit port map(left => left, right => right, operation => operation, input => input, output => output);
	stim_proc: process
	begin

		-- NOTE: the left/right input bits works, but they are currently tied to 0 in the functional unit

		-- Test #1 - No operation
		input <= x"80010001"; left <= '0'; right <= '0'; operation <= "00"; wait for 25 ns;
		assert output = x"80010001" report "shifter_32bit test #1" severity failure;

		-- Test #2 - Shift left
		input <= x"80010001"; left <= '0'; right <= '0'; operation <= "10"; wait for 25 ns;
		assert output = x"00020002" report "shifter_32bit test #2" severity failure;

		-- Shift right
		input <= x"80010001"; left <= '0'; right <= '0'; operation <= "01"; wait for 25 ns;
		assert output = x"40008000" report "shifter_32bit test #3" severity failure;
		
		-- Shift left, input 1 into LSB
		input <= x"80010001"; left <= '1'; right <= '0'; operation <= "10"; wait for 25 ns;
		assert output = x"00020003" report "shifter_32bit test #4" severity failure;

		-- Shift right, input 1 to MSB
		input <= x"80010001"; left <= '0'; right <= '1'; operation <= "01"; wait for 25 ns;
		assert output = x"c0008000" report "shifter_32bit test #5" severity failure;

		std.env.finish;

	end process;
end;

