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
		-- NOTE: the left/right input bits works, but they are currently fixed to 0 in the functional unit
		left  <= '0'; -- this bit gets shifted in when shifting left
		right <= '0'; -- this bit gets shifted in when shifting right
		input <= x"80010001";
		operation <= "00";
		wait for 20 ns;

		-- Shift left
		operation <= "10";
		wait for 20 ns;

		-- Shift right
		operation <= "01";
		wait for 20 ns;
		
		-- Shift left, input 1 to LSB
		left  <= '1';
		right <= '0';
		operation <= "10";
		wait for 20 ns;

		-- Shift right, input 1 to MSB
		left  <= '0';
		right <= '1';
		operation <= "01";
		wait for 20 ns;

		-- Idle
		left  <= '0';
		right <= '0';
		operation <= "00";
		wait;

	end process;
end;

