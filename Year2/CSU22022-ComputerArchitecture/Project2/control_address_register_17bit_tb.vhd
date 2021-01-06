library ieee;
use ieee.std_logic_1164.all;

entity control_address_register_17bit_tb is
end control_address_register_17bit_tb;

architecture Behavior of control_address_register_17bit_tb is

	component control_address_register_17bit is
		port(
			clock, reset      : in std_logic;
			increment_disable : in std_logic;
			input_addr        : in std_logic_vector(16 downto 0);
			output            : out std_logic_vector(16 downto 0));
	end component;

	signal clock             : std_logic := '0';
	signal reset             : std_logic;
	signal increment_disable : std_logic;
	signal input_addr        : std_logic_vector(16 downto 0);
	signal output            : std_logic_vector(16 downto 0);

begin

	-- Simulate a 1 MHz clock
	clock <= not clock after 500 ns;

	uut: control_address_register_17bit port map(
		clock => clock, reset => reset,
		increment_disable => increment_disable,
		input_addr => input_addr,
		output => output);

	stim_proc: process
	begin

		-- Test #1 - Reset signal without increment
		reset <= '1';
		wait until falling_edge(clock);
		assert output = "00000000000000001" report "control_address_register_17bit test #1" severity failure;
		reset <= '0';

		-- Test #2 - Load without increment
		increment_disable <= '1';
		input_addr <= "00000000000000001";
		wait until falling_edge(clock);
		assert output = "00000000000000001" report "control_address_register_17bit test #2" severity failure;

		-- Test #3 - Load with increment
		increment_disable <= '0';
		input_addr <= "00000000000000011";
		wait until falling_edge(clock);
		assert output = "00000000000000100" report "control_address_register_17bit test #3" severity failure;

		std.env.finish;

	end process;
end;

