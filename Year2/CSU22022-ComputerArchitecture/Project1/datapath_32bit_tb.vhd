library ieee;
use ieee.std_logic_1164.all;

entity datapath_32bit_tb is
end datapath_32bit_tb;

architecture Behavior of datapath_32bit_tb is
	component datapath_32bit
		port(
			clock, load_enable, use_constant, load_from_mem    : in std_logic;
			function_select, dst_address, a_address, b_address : in std_logic_vector(4 downto 0);
			constant_in, input_data                            : in std_logic_vector(31 downto 0);
			address_out, data_out                              : out std_logic_vector(31 downto 0);
			v, c, n, z                                         : out std_logic);
	end component;

	signal clock                                              : std_logic := '0';
	signal load_enable, use_constant, load_from_mem           : std_logic;
	signal function_select, dst_address, a_address, b_address : std_logic_vector(4 downto 0);
	signal constant_in, input_data                            : std_logic_vector(31 downto 0);
	signal address_out, data_out                              : std_logic_vector(31 downto 0);
	signal v, c, n, z                                         : std_logic;

begin

	-- Simulate 1 MHz clock - The maximum propagation delay for the circuit is 500 ns, so 1 MHz allows just enough time
	clock <= not clock after 500 ns;

	uut: datapath_32bit port map(
		clock => clock, load_enable => load_enable, use_constant => use_constant, load_from_mem => load_from_mem,
		function_select => function_select, dst_address => dst_address, a_address => a_address, b_address => b_address,
		constant_in => constant_in, input_data => input_data, address_out => address_out, data_out => data_out,
		v => v, c => c, n => n, z => z);
	stim_proc: process
	begin
	
		-- NOTE: All registers start completely uninitialized, so all there outputs are initially undefined!
		-- NOTE: We execute on the falling-edge, and write to the registers on the rising-edge
		 
		-- For the first clock cycle, we won't do anything
		wait until falling_edge(clock);
		
		-- During this clock cycle, we load 0x00000010 into reg01, set bus A to reg01 output
		load_enable <= '1';         -- Allow writing into register file
		use_constant <= '0';        -- Does not matter
		constant_in <= x"00000000"; -- Does not matter
		load_from_mem <= '1';       -- Load input_data by selecting line1 in mux_md
		input_data <= x"00000010";  -- Load 0x00000010 as input_data
		function_select <= "00000"; -- Does not matter
		a_address <= "00001";       -- Does not matter, but we select reg01 as the bus A input, just so we can confirm the load was successful
		b_address <= "00001";       -- Does not matter
		dst_address <= "00001";     -- Select reg01 as the destination
		wait until falling_edge(clock);
		
		-- Add 0x00000020 and reg01, place result in reg02
		load_enable <= '1';         -- Allow writing into register file
		use_constant <= '1';        -- Use a constant value instead of bus B as function unit input B
		constant_in <= x"00000020"; -- Use 0x00000020 as constant value
		load_from_mem <= '0';       -- Save output of functional unit instead of loading input_data to the register file
		input_data <= x"00000000";  -- Does not matter
		function_select <= "00010"; -- ADDITION as the operation
		a_address <= "00001";       -- Select reg01 as the bus A input
		b_address <= "00010";       -- Does not matter, but we select reg02 as the bus B input, just so we can confirm the sum was successful
		dst_address <= "00010";     -- Select reg02 as the destination
		wait until falling_edge(clock);
		
		-- Add reg01 and reg02, place result in reg1f
		load_enable <= '1';         -- Allow writing into register file
		use_constant <= '0';        -- Use bus B as function unit input B, not constant_in
		constant_in <= x"00000000"; -- Does not matter
		load_from_mem <= '0';       -- Save output of functional unit instead of loading input_data to the register file
		input_data <= x"00000000";  -- Does not matter
		function_select <= "00010"; -- ADDITION as the operation
		a_address <= "00001";       -- Select reg01 as the bus A input
		b_address <= "00010";       -- Select reg02 as the bus B input
		dst_address <= "11111";     -- Select reg1f as the destination
		wait until falling_edge(clock);
		
		-- Transfer reg1f to reg1f just to observe if the addition was successful
		load_enable <= '1';         -- Allow writing into register file
		use_constant <= '0';        -- Use bus B as function unit input B, not constant_in
		constant_in <= x"00000000"; -- Does not matter
		load_from_mem <= '0';       -- Save output of functional unit instead of loading input_data to the register file
		input_data <= x"00000000";  -- Does not matter
		function_select <= "00000"; -- TRANSFER as the operation
		a_address <= "11111";       -- Select reg1f as the bus A input
		b_address <= "00010";       -- Does not matter
		dst_address <= "11111";     -- Select reg1f as the destination
		wait until falling_edge(clock);
		
		-- Subtract 0x00000005 from reg1f and place in reg1f
		load_enable <= '1';
		use_constant <= '1';        -- Use immediate value
		constant_in <= x"00000005"; -- b: 0x05
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "00101"; -- Subtraction
		a_address <= "11111";       -- a: reg1f
		b_address <= "00010";
		dst_address <= "11111";     -- dst: reg1f
		wait until falling_edge(clock);
		
		-- AND 0x00000003 with reg1f and place in reg1f
		load_enable <= '1';
		use_constant <= '1';        -- Use immediate value
		constant_in <= x"00000003"; -- b: 0x03
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "01000"; -- AND
		a_address <= "11111";       -- a: reg1f
		b_address <= "00010";
		dst_address <= "11111";     -- dst: reg1f
		wait until falling_edge(clock);
		
		-- Shift reg1f left
		load_enable <= '1';
		use_constant <= '0';
		constant_in <= x"00000000";
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "11000"; -- Shift left
		a_address <= "11111";
		b_address <= "11111";       -- b: reg1f
		dst_address <= "11111";     -- dst: reg1f
		wait until falling_edge(clock);
		
		
		-- Demonstrate ALU vcnz flags
		
		-- Load 0x7fffffff to reg00
		load_enable <= '1';
		use_constant <= '0';
		constant_in <= x"00000000";
		load_from_mem <= '1';       -- Write value to register from memory
		input_data <= x"7fffffff";  -- Loading 0x7fffffff from 'memory'
		function_select <= "00000";
		a_address <= "00000";       -- Write out reg00 (for observing result only)
		b_address <= "00000";       -- Write out reg00 (for observing result only)
		dst_address <= "00000";     -- dst: reg00
		wait until falling_edge(clock);

		-- Add w/ carry 0x7fffffff to reg00 to set the oVerflow and Negative flags
		load_enable <= '1';
		use_constant <= '1';        -- Use immediate value
		constant_in <= x"7fffffff"; -- b: 0x7fffffff
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "00011"; -- Add w/ carry
		a_address <= "00000";       -- a: reg00
		b_address <= "00000";
		dst_address <= "00000";     -- dst: reg00
		wait until falling_edge(clock);

		-- Add 0x01 to reg00 to set the Carry and Zero flags
		load_enable <= '1';
		use_constant <= '1';        -- Use immediate value
		constant_in <= x"00000001"; -- b: 0x00000001
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "00010"; -- Add
		a_address <= "00000";       -- a: reg00
		b_address <= "00000";
		dst_address <= "00000";     -- dst: reg00
		wait until falling_edge(clock);
		
		-- Idle
		load_enable <= '0';
		use_constant <= '0';
		constant_in <= x"00000000";
		load_from_mem <= '0';
		input_data <= x"00000000";
		function_select <= "00000";
		a_address <= "00000";
		b_address <= "00000";
		dst_address <= "00000";
		wait;
		
	end process;
end;

