library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file_32bit_tb is
end register_file_32bit_tb;

architecture Behavior of register_file_32bit_tb is
	component register_file_32bit
		port(
			clock, load_enable             : in std_logic;
			dst_select, a_select, b_select : in std_logic_vector(4 downto 0);
			input_data                     : in std_logic_vector(31 downto 0);
			a_output_data, b_output_data   : out std_logic_vector(31 downto 0));
	end component;

	signal clock : std_logic := '0';
	signal load_enable : std_logic;
	signal dst_select, a_select, b_select : std_logic_vector(4 downto 0);
	signal input_data, a_output_data, b_output_data   : std_logic_vector(31 downto 0);

begin

	-- Simulate a 10 MHz clock
	clock <= not clock after 50 ns;

	uut: register_file_32bit port map(
		clock => clock, load_enable => load_enable,
		dst_select => dst_select, a_select => a_select, b_select => b_select,
		input_data => input_data,
		a_output_data => a_output_data, b_output_data => b_output_data);
	stim_proc: process
	begin

		-- All register start uninitialized, so there output is also undefined
		-- Do nothing for the first clock cycle, expect madness with everything uninitialized
		wait for 100 ns;

		-- Simulate loading 0x00000001 into reg01, write reg01 to bus A
		load_enable <= '1';
		a_select <= "00001";
		dst_select <= "00001";
		input_data <= x"00000001";
		wait for 100 ns;

		-- Simulate loading 0x0f0f0f0f into reg05, write reg05 to bus B
		b_select <= "00101";
		dst_select <= "00101";
		input_data <= x"0f0f0f0f";
		wait for 100 ns;

		-- Overwrite the value in reg05 with 0xffffffff
		input_data <= x"ffffffff";
		wait for 100 ns;
		
		-- Disable loading for this clock cycle, change bus A to output of reg05 as well
		load_enable <= '0';
		a_select <= "00101";
		wait for 100 ns;

		-- Load 0x55555555 into reg1f without outputting to a bus
		load_enable <= '1';
		dst_select <= "11111";
		input_data <= x"55555555";
		wait for 100 ns;

		-- Set output of bus A and B to reg1f
		load_enable <= '0';
		a_select <= "11111";
		b_select <= "11111";
		wait;

	end process;
end;

