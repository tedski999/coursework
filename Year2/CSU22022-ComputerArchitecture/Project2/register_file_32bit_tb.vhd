library ieee;
use ieee.std_logic_1164.all;

entity register_file_32bit_tb is
end register_file_32bit_tb;

architecture Behavior of register_file_32bit_tb is
	component register_file_32bit
		port(
			clock, load_enable           : in std_logic;
			td_dr, ta_sa, tb_sb          : in std_logic_vector(5 downto 0);
			input_data                   : in std_logic_vector(31 downto 0);
			a_output_data, b_output_data : out std_logic_vector(31 downto 0));
	end component;

	signal clock                          : std_logic := '0';
	signal load_enable                    : std_logic;
	signal td_dr, ta_sa, tb_sb            : std_logic_vector(5 downto 0);
	signal input_data                     : std_logic_vector(31 downto 0);
	signal a_output_data, b_output_data   : std_logic_vector(31 downto 0);

begin

	-- Simulate a 1 MHz clock
	clock <= not clock after 500 ns;

	uut: register_file_32bit port map(
		clock => clock, load_enable => load_enable,
		td_dr => td_dr, ta_sa => ta_sa, tb_sb => tb_sb,
		input_data => input_data,
		a_output_data => a_output_data, b_output_data => b_output_data);
	stim_proc: process
	begin

		-- NOTE: All register start uninitialized, so their output is initially undefined.
		-- NOTE: The dst register is written to on the rising edge of the clock.

		wait until falling_edge(clock);

		-- Test #1 - Write 0x00000001 into reg01, read reg01 onto bus A and B
		load_enable <= '1';
		input_data <= x"00000001";
		td_dr <= "000001";
		ta_sa <= "000001";
		tb_sb <= "000001";
		wait until falling_edge(clock);
		assert a_output_data = x"00000001" report "register_file_32bit test #1 - bus a" severity failure;
		assert b_output_data = x"00000001" report "register_file_32bit test #1 - bus b" severity failure;

		-- Test #2 - Write 0x0f0f0f0f into reg05, read reg01 onto bus A, read reg05 onto bus B
		load_enable <= '1';
		input_data <= x"0f0f0f0f";
		ta_sa <= "000001";
		tb_sb <= "000101";
		td_dr <= "000101";
		wait until falling_edge(clock);
		assert a_output_data = x"00000001" report "register_file_32bit test #2 - bus a" severity failure;
		assert b_output_data = x"0f0f0f0f" report "register_file_32bit test #2 - bus b" severity failure;

		-- Test #3 - Write 0xffffffff into reg05, read reg01 onto bus A, read reg05 onto bus B
		load_enable <= '1';
		input_data <= x"ffffffff";
		ta_sa <= "000001";
		tb_sb <= "000101";
		td_dr <= "000101";
		wait until falling_edge(clock);
		assert a_output_data = x"00000001" report "register_file_32bit test #3 - bus a" severity failure;
		assert b_output_data = x"ffffffff" report "register_file_32bit test #3 - bus b" severity failure;
		
		-- Test #4 - Don't write, read reg01 onto bus A, read reg05 onto bus B
		load_enable <= '0';
		input_data <= x"00000000";
		ta_sa <= "000001";
		tb_sb <= "000101";
		td_dr <= "000101";
		wait until falling_edge(clock);
		assert a_output_data = x"00000001" report "register_file_32bit test #4 - bus a" severity failure;
		assert b_output_data = x"ffffffff" report "register_file_32bit test #4 - bus b" severity failure;

		-- Test #5 - Write 0x12345678 to temp reg, read reg01 onto bus A, read reg05 onto bus B
		load_enable <= '1';
		input_data <= x"12345678";
		ta_sa <= "000001";
		tb_sb <= "000101";
		td_dr <= "100000";
		wait until falling_edge(clock);
		assert a_output_data = x"00000001" report "register_file_32bit test #5 - bus a" severity failure;
		assert b_output_data = x"ffffffff" report "register_file_32bit test #5 - bus b" severity failure;

		-- Test #6 - Don't write, read temp reg onto bus A and B
		load_enable <= '0';
		input_data <= x"00000000";
		ta_sa <= "100000";
		tb_sb <= "100000";
		td_dr <= "100000";
		wait until falling_edge(clock);
		assert a_output_data = x"12345678" report "register_file_32bit test #6 - bus a" severity failure;
		assert b_output_data = x"12345678" report "register_file_32bit test #6 - bus b" severity failure;

		std.env.finish;

	end process;
end;

