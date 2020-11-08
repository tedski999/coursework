library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file_tb is
end register_file_tb;

architecture Behavior of register_file_tb is
	component register_file
		port(
			clk, load        : in std_logic;
			src_reg, dst_reg : in std_logic_vector(4 downto 0);
			input            : in std_logic_vector(31 downto 0);
			output00, output01, output02, output03 : inout std_logic_vector(31 downto 0);
			output04, output05, output06, output07 : inout std_logic_vector(31 downto 0);
			output08, output09, output0a, output0b : inout std_logic_vector(31 downto 0);
			output0c, output0d, output0e, output0f : inout std_logic_vector(31 downto 0);
			output10, output11, output12, output13 : inout std_logic_vector(31 downto 0);
			output14, output15, output16, output17 : inout std_logic_vector(31 downto 0);
			output18, output19, output1a, output1b : inout std_logic_vector(31 downto 0);
			output1c, output1d, output1e, output1f : inout std_logic_vector(31 downto 0));
	end component;

	signal clk, load        : std_logic := '0';
	signal src_reg, dst_reg : std_logic_vector(4 downto 0) := (others => '0');
	signal input            : std_logic_vector(31 downto 0) := (others => '0');
	signal output00, output01, output02, output03 : std_logic_vector(31 downto 0) := (others => '0');
	signal output04, output05, output06, output07 : std_logic_vector(31 downto 0) := (others => '0');
	signal output08, output09, output0a, output0b : std_logic_vector(31 downto 0) := (others => '0');
	signal output0c, output0d, output0e, output0f : std_logic_vector(31 downto 0) := (others => '0');
	signal output10, output11, output12, output13 : std_logic_vector(31 downto 0) := (others => '0');
	signal output14, output15, output16, output17 : std_logic_vector(31 downto 0) := (others => '0');
	signal output18, output19, output1a, output1b : std_logic_vector(31 downto 0) := (others => '0');
	signal output1c, output1d, output1e, output1f : std_logic_vector(31 downto 0) := (others => '0');

begin
	uut: register_file port map(
		clk => clk, load => load, src_reg => src_reg, dst_reg => dst_reg, input => input,
		output00 => output00, output01 => output01, output02 => output02, output03 => output03,
		output04 => output04, output05 => output05, output06 => output06, output07 => output07,
		output08 => output08, output09 => output09, output0a => output0a, output0b => output0b,
		output0c => output0c, output0d => output0d, output0e => output0e, output0f => output0f,
		output10 => output10, output11 => output11, output12 => output12, output13 => output13,
		output14 => output14, output15 => output15, output16 => output16, output17 => output17,
		output18 => output18, output19 => output19, output1a => output1a, output1b => output1b,
		output1c => output1c, output1d => output1d, output1e => output1e, output1f => output1f);
	
	-- 100 MHz clock
	process
	begin
		clk <= not clk after 5 ns;
		wait for 5 ns;
	end process;
	
	stim_proc: process
	begin

		src_reg <= "00000";
		dst_reg <= "00000";
		wait for 50 ns;

		-- Loading HEX values into the 32 registers
		load <= '1';
		dst_reg <= "00000"; input <= x"012709c2"; wait for 10 ns;
		dst_reg <= "00001"; input <= x"012709c1"; wait for 10 ns;
		dst_reg <= "00010"; input <= x"012709c0"; wait for 10 ns;
		dst_reg <= "00011"; input <= x"012709bf"; wait for 10 ns;
		dst_reg <= "00100"; input <= x"012709be"; wait for 10 ns;
		dst_reg <= "00101"; input <= x"012709bd"; wait for 10 ns;
		dst_reg <= "00110"; input <= x"012709bc"; wait for 10 ns;
		dst_reg <= "00111"; input <= x"012709bb"; wait for 10 ns;
		dst_reg <= "01000"; input <= x"012709ba"; wait for 10 ns;
		dst_reg <= "01001"; input <= x"012709b9"; wait for 10 ns;
		dst_reg <= "01010"; input <= x"012709b8"; wait for 10 ns;
		dst_reg <= "01011"; input <= x"012709b7"; wait for 10 ns;
		dst_reg <= "01100"; input <= x"012709b6"; wait for 10 ns;
		dst_reg <= "01101"; input <= x"012709b5"; wait for 10 ns;
		dst_reg <= "01110"; input <= x"012709b4"; wait for 10 ns;
		dst_reg <= "01111"; input <= x"012709b3"; wait for 10 ns;
		dst_reg <= "10000"; input <= x"012709b2"; wait for 10 ns;
		dst_reg <= "10001"; input <= x"012709b1"; wait for 10 ns;    
		dst_reg <= "10010"; input <= x"012709a0"; wait for 10 ns;    
		dst_reg <= "10011"; input <= x"012709af"; wait for 10 ns;    
		dst_reg <= "10100"; input <= x"012709ae"; wait for 10 ns;    
		dst_reg <= "10101"; input <= x"012709ad"; wait for 10 ns;    
		dst_reg <= "10110"; input <= x"012709ac"; wait for 10 ns;    
		dst_reg <= "10111"; input <= x"012709ab"; wait for 10 ns;    
		dst_reg <= "11000"; input <= x"012709aa"; wait for 10 ns;    
		dst_reg <= "11001"; input <= x"012709a9"; wait for 10 ns;    
		dst_reg <= "11010"; input <= x"012709a8"; wait for 10 ns;    
		dst_reg <= "11011"; input <= x"012709a7"; wait for 10 ns;    
		dst_reg <= "11100"; input <= x"012709a6"; wait for 10 ns;    
		dst_reg <= "11101"; input <= x"012709a5"; wait for 10 ns;    
		dst_reg <= "11110"; input <= x"012709a4"; wait for 10 ns;    
		dst_reg <= "11111"; input <= x"012709a3"; wait for 10 ns;

		-- Transfering register 00 to 01, 01 to 02, 02 to 03, etc... for 10 examples
		load <= '0';
		src_reg <= "00000"; dst_reg <= "00001"; wait for 10 ns;
		src_reg <= "00001"; dst_reg <= "00010"; wait for 10 ns;
		src_reg <= "00010"; dst_reg <= "00011"; wait for 10 ns;
		src_reg <= "00011"; dst_reg <= "00100"; wait for 10 ns;
		src_reg <= "00100"; dst_reg <= "00101"; wait for 10 ns;
		src_reg <= "00101"; dst_reg <= "00110"; wait for 10 ns;
		src_reg <= "00110"; dst_reg <= "00111"; wait for 10 ns;
		src_reg <= "00111"; dst_reg <= "01000"; wait for 10 ns;
		src_reg <= "01000"; dst_reg <= "01001"; wait for 10 ns;
		src_reg <= "01001"; dst_reg <= "01010"; wait for 10 ns;
		
		wait for 1000 ns;
		
	end process;
end;

