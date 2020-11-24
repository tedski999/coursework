library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity datapath_32bit_tb is
end datapath_32bit_tb;

architecture Behavior of datapath_32bit_tb is
	component datapath_32bit
		port(
			clock, load_enable, mb_select, md_select           : in std_logic;
			function_select, dst_address, a_address, b_address : in std_logic_vector(4 downto 0);
			constant_in, input_data                            : in std_logic_vector(31 downto 0);
			address_out, data_out                              : out std_logic_vector(31 downto 0);
			v, c, n, z                                         : out std_logic);
	end component;

	signal clock : std_logic := '0';
	signal load_enable, mb_select, md_select : std_logic;
	signal function_select, dst_address, a_address, b_address : std_logic_vector(4 downto 0);
	signal constant_in, input_data : std_logic_vector(31 downto 0);
	signal address_out, data_out : std_logic_vector(31 downto 0);
	signal v, c, n, z : std_logic;

begin

	-- Simulate 10 MHz clock
	clock <= not clock after 50 ns;

	uut: datapath_32bit port map(
		clock => clock, load_enable => load_enable, mb_select => mb_select, md_select => md_select,
		function_select => function_select, dst_address => dst_address, a_address => a_address, b_address => b_address,
		constant_in => constant_in, input_data => input_data, address_out => address_out, data_out => data_out,
		v => v, c => c, n => n, z => z);
	stim_proc: process
	begin
	
		-- Remember, all registers start completely uninitialized, so all there outputs are initially undefined!
		-- For the first clock cycle, we won't do anything
		wait for 100 ns;
		
		-- During this clock cycle, we load 0x00000010 into reg01, set bus A to reg01 output
		load_enable <= '1';         -- Allow writing into register file
		md_select <= '1';           -- Load input_data by selecting line1 in mux_md
		dst_address <= "00001";     -- Select reg01 as the destination
		a_address <= "00001";       -- Select reg01 as the bus A input
		input_data <= x"00000010";  -- Load 0x00000010 as input_data
		wait for 100 ns;
		
		-- Add value 0x00000020 and reg01, place result in reg02
		mb_select <= '1';           -- Use a constant value instead of bus B as function unit input B
		md_select <= '0';           -- Save output of functional unit instead of input_data to the register file
		constant_in <= x"00000020"; -- Use 0x00000020 as constant value
		a_address <= "00001";       -- Select reg01 as the bus A input
		dst_address <= "00010";     -- Select reg02 as the destination
		function_select <= "00000"; -- Set functional unit to perform addition
		wait for 100 ns;
		
		-- Add reg01 and reg02, place result in reg1f
		mb_select <= '0';           -- Use bus B as function unit input B
		a_address <= "00001";       -- Select reg01 as the bus A input
		b_address <= "00010";       -- Select reg02 as the bus B input
		dst_address <= "11111";     -- Select reg1f as the destination
		function_select <= "00000"; -- Set functional unit to perform addition
		wait;
		
	end process;
end;

