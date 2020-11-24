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
	signal operation : std_logic_vector(1 downto 0);
	signal input, output : std_logic_vector(31 downto 0);

begin
	uut: shifter_32bit port map(left => left, right => right, operation => operation, input => input, output => output);
	stim_proc: process
	begin
		-- TODO: check if shifting right/left works
		wait;
	end process;
end;

