library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity processor_32bit_tb is
end processor_32bit_tb;

architecture Behavior of processor_32bit_tb is
	component processor_32bit
		port(
			clock, mb_select, md_select                        : in std_logic;
			function_select, dst_address, a_address, b_address : in std_logic_vector(4 downto 0);
			constant_in, input_data                            : in std_logic_vector(31 downto 0);
			address_out, data_out                              : out std_logic_vector(31 downto 0);
			v, c, n, z                                         : out std_logic);
	end component;

	signal clock, mb_select, md_select : std_logic;
	signal function_select, dst_address, a_address, b_address : std_logic_vector(4 downto 0);
	signal constant_in, input_data : std_logic_vector(31 downto 0);
	signal address_out, data_out : std_logic_vector(31 downto 0);
	signal v, c, n, z : std_logic;

begin
	uut: processor_32bit port map(
		clock => clock, mb_select => mb_select, md_select => md_select,
		function_select => function_select, dst_address => dst_address, a_address => a_address, b_address => b_address,
		constant_in => constant_in, input_data => input_data,
		v => v, c => c, n => n, z => z);
	stim_proc: process
	begin

		clock <= '0';
		mb_select <= '0';
		md_select <= '0';
		function_select <= "00000";
		dst_address <= "00000";
		a_address <= "00000";
		b_address <= "00000";
		constant_in <= x"00000000";
		input_data <= x"00000000";

		wait;
	end process;
end;

