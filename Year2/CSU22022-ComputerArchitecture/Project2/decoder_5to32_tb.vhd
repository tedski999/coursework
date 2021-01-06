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
		input <= "00000"; wait for 25 ns; assert output = x"00000001" report "decoder_5to32 test #1" severity failure;
		input <= "00001"; wait for 25 ns; assert output = x"00000002" report "decoder_5to32 test #2" severity failure;
		input <= "00010"; wait for 25 ns; assert output = x"00000004" report "decoder_5to32 test #3" severity failure;
		input <= "00011"; wait for 25 ns; assert output = x"00000008" report "decoder_5to32 test #4" severity failure;
		input <= "00100"; wait for 25 ns; assert output = x"00000010" report "decoder_5to32 test #5" severity failure;
		input <= "00101"; wait for 25 ns; assert output = x"00000020" report "decoder_5to32 test #6" severity failure;
		input <= "00110"; wait for 25 ns; assert output = x"00000040" report "decoder_5to32 test #7" severity failure;
		input <= "00111"; wait for 25 ns; assert output = x"00000080" report "decoder_5to32 test #8" severity failure;
		input <= "01000"; wait for 25 ns; assert output = x"00000100" report "decoder_5to32 test #9" severity failure;
		input <= "01001"; wait for 25 ns; assert output = x"00000200" report "decoder_5to32 test #10" severity failure;
		input <= "01010"; wait for 25 ns; assert output = x"00000400" report "decoder_5to32 test #11" severity failure;
		input <= "01011"; wait for 25 ns; assert output = x"00000800" report "decoder_5to32 test #12" severity failure;
		input <= "01100"; wait for 25 ns; assert output = x"00001000" report "decoder_5to32 test #13" severity failure;
		input <= "01101"; wait for 25 ns; assert output = x"00002000" report "decoder_5to32 test #14" severity failure;
		input <= "01110"; wait for 25 ns; assert output = x"00004000" report "decoder_5to32 test #15" severity failure;
		input <= "01111"; wait for 25 ns; assert output = x"00008000" report "decoder_5to32 test #16" severity failure;
		input <= "10000"; wait for 25 ns; assert output = x"00010000" report "decoder_5to32 test #17" severity failure;
		input <= "10001"; wait for 25 ns; assert output = x"00020000" report "decoder_5to32 test #18" severity failure;
		input <= "10010"; wait for 25 ns; assert output = x"00040000" report "decoder_5to32 test #19" severity failure;
		input <= "10011"; wait for 25 ns; assert output = x"00080000" report "decoder_5to32 test #20" severity failure;
		input <= "10100"; wait for 25 ns; assert output = x"00100000" report "decoder_5to32 test #21" severity failure;
		input <= "10101"; wait for 25 ns; assert output = x"00200000" report "decoder_5to32 test #22" severity failure;
		input <= "10110"; wait for 25 ns; assert output = x"00400000" report "decoder_5to32 test #23" severity failure;
		input <= "10111"; wait for 25 ns; assert output = x"00800000" report "decoder_5to32 test #24" severity failure;
		input <= "11000"; wait for 25 ns; assert output = x"01000000" report "decoder_5to32 test #25" severity failure;
		input <= "11001"; wait for 25 ns; assert output = x"02000000" report "decoder_5to32 test #26" severity failure;
		input <= "11010"; wait for 25 ns; assert output = x"04000000" report "decoder_5to32 test #27" severity failure;
		input <= "11011"; wait for 25 ns; assert output = x"08000000" report "decoder_5to32 test #28" severity failure;
		input <= "11100"; wait for 25 ns; assert output = x"10000000" report "decoder_5to32 test #29" severity failure;
		input <= "11101"; wait for 25 ns; assert output = x"20000000" report "decoder_5to32 test #30" severity failure;
		input <= "11110"; wait for 25 ns; assert output = x"40000000" report "decoder_5to32 test #31" severity failure;
		input <= "11111"; wait for 25 ns; assert output = x"80000000" report "decoder_5to32 test #32" severity failure;
		std.env.finish;
	end process;
end;

