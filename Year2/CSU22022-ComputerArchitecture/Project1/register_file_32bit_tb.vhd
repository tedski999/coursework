library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file_32bit_tb is
end register_file_32bit_tb;

architecture Behavior of register_file_32bit_tb is
	component register_file_32bit
		port(
			load_enable                    : in std_logic;
			dst_select, a_select, b_select : in std_logic_vector(4 downto 0);
			input_data                     : in std_logic_vector(31 downto 0);
			a_output_data, b_output_data   : out std_logic_vector(31 downto 0));
	end component;

	signal load_enable : std_logic;
	signal dst_select, a_select, b_select : std_logic_vector(4 downto 0);
	signal input_data                     : std_logic_vector(31 downto 0);
	signal a_output_data, b_output_data   : std_logic_vector(31 downto 0);

begin
	uut: register_file_32bit port map(
		load_enable => load_enable,
		dst_select => dst_select, a_select => a_select, b_select => b_select,
		input_data => input_data,
		a_output_data => a_output_data, b_output_data => b_output_data);
	stim_proc: process
	begin

		-- all register start uninitialized, so there output is also undefined
		load_enable <= '0';
		a_select <= "00000";
		b_select <= "00000";
		wait for 10 ns;

		-- simulate loading 0x00000000 into reg00 while observing with a
		a_select <= "00000";
		dst_select <= "00000";
		input_data <= x"00000000";
		wait for 10 ns; load_enable <= '1';
		wait for 10 ns; load_enable <= '0';

		wait for 50 ns;

		-- simulate loading 0x0f0f0f0f into reg05 while observing with b
		b_select <= "00101";
		dst_select <= "00101";
		input_data <= x"0f0f0f0f";
		wait for 10 ns; load_enable <= '1';
		wait for 10 ns; load_enable <= '0';

		wait for 50 ns;

		-- load a different value 0xffffffff into reg05
		dst_select <= "00101";
		input_data <= x"ffffffff";
		wait for 10 ns; load_enable <= '1';
		wait for 10 ns; load_enable <= '0';

		wait for 50 ns;

		-- load 0x55555555 into reg1f without observing
		dst_select <= "11111";
		input_data <= x"55555555";
		wait for 10 ns; load_enable <= '1';
		wait for 10 ns; load_enable <= '0';

		wait for 50 ns;

		-- set output of a and b to reg1f
		a_select <= "11111";
		b_select <= "11111";

		wait;
	end process;
end;

