library ieee;
use ieee.std_logic_1164.all;

entity decoder_5to32_tb is
end decoder_5to32_tb;

architecture Behavior of decoder_5to32_tb is
	component decoder_5to32
		port(
			input  : in std_logic_vector(4 downto 0);
			output : out std_logic_vector(31 downto 0));
	end component;

	signal input  : std_logic_vector(4 downto 0);
	signal output : std_logic_vector(31 downto 0);

begin
	uut: decoder_5to32 port map(input => input, output => output);
	stim_proc: process
	begin
		input <= "00000"; wait for 10 ns;
		input <= "00001"; wait for 10 ns;
		input <= "00010"; wait for 10 ns;
		input <= "00011"; wait for 10 ns;
		input <= "00100"; wait for 10 ns;
		input <= "00101"; wait for 10 ns;
		input <= "00110"; wait for 10 ns;
		input <= "00111"; wait for 10 ns;
		input <= "01000"; wait for 10 ns;
		input <= "01001"; wait for 10 ns;
		input <= "01010"; wait for 10 ns;
		input <= "01011"; wait for 10 ns;
		input <= "01100"; wait for 10 ns;
		input <= "01101"; wait for 10 ns;
		input <= "01110"; wait for 10 ns;
		input <= "01111"; wait for 10 ns;
		input <= "10000"; wait for 10 ns;
		input <= "10001"; wait for 10 ns;
		input <= "10010"; wait for 10 ns;
		input <= "10011"; wait for 10 ns;
		input <= "10100"; wait for 10 ns;
		input <= "10101"; wait for 10 ns;
		input <= "10110"; wait for 10 ns;
		input <= "10111"; wait for 10 ns;
		input <= "11000"; wait for 10 ns;
		input <= "11001"; wait for 10 ns;
		input <= "11010"; wait for 10 ns;
		input <= "11011"; wait for 10 ns;
		input <= "11100"; wait for 10 ns;
		input <= "11101"; wait for 10 ns;
		input <= "11110"; wait for 10 ns;
		input <= "11111"; wait;
	end process;
end;

