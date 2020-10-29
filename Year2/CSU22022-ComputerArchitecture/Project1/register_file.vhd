library ieee;
use ieee.std_logic_1164.all;

entity register_file is
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
end register_file;

architecture Behavioral of register_file is

	component decoder_5to32
		port(
			s : in  std_logic_vector(4 downto 0);
			o : out std_logic_vector(31 downto 0));
	end component;

	component mux2_32
		port(
			s        : in  std_logic;
			in0, in1 : in  std_logic_vector(31 downto 0);
			o        : out std_logic_vector(31 downto 0));
	end component;

	component reg32
		port(
			load, clk : in  std_logic;
			d         : in  std_logic_vector(31 downto 0);
			o         : out std_logic_vector(31 downto 0));
	end component;

	component mux32_32
		port(
			s : in std_logic_vector(4 downto 0);
			in00, in01, in02, in03 : in std_logic_vector(31 downto 0);
			in04, in05, in06, in07 : in std_logic_vector(31 downto 0);
			in08, in09, in0a, in0b : in std_logic_vector(31 downto 0);
			in0c, in0d, in0e, in0f : in std_logic_vector(31 downto 0);
			in10, in11, in12, in13 : in std_logic_vector(31 downto 0);
			in14, in15, in16, in17 : in std_logic_vector(31 downto 0);
			in18, in19, in1a, in1b : in std_logic_vector(31 downto 0);
			in1c, in1d, in1e, in1f : in std_logic_vector(31 downto 0);
			o : out std_logic_vector(31 downto 0));
	end component;

	signal load_bitmap, loopback, reg_input : std_logic_vector(31 downto 0) := (others => '0');

begin

	dst_decoder: decoder_5to32 port map(s => dst_reg, o => load_bitmap);
	src_mux: mux2_32 port map(s => load, in0 => loopback, in1 => input, o => reg_input);
	reg00: reg32 port map(d => reg_input, load => load_bitmap(0),  clk => clk, o => output00);
	reg01: reg32 port map(d => reg_input, load => load_bitmap(1),  clk => clk, o => output01);
	reg02: reg32 port map(d => reg_input, load => load_bitmap(2),  clk => clk, o => output02);
	reg03: reg32 port map(d => reg_input, load => load_bitmap(3),  clk => clk, o => output03);
	reg04: reg32 port map(d => reg_input, load => load_bitmap(4),  clk => clk, o => output04);
	reg05: reg32 port map(d => reg_input, load => load_bitmap(5),  clk => clk, o => output05);
	reg06: reg32 port map(d => reg_input, load => load_bitmap(6),  clk => clk, o => output06);
	reg07: reg32 port map(d => reg_input, load => load_bitmap(7),  clk => clk, o => output07);
	reg08: reg32 port map(d => reg_input, load => load_bitmap(8),  clk => clk, o => output08);
	reg09: reg32 port map(d => reg_input, load => load_bitmap(9),  clk => clk, o => output09);
	reg10: reg32 port map(d => reg_input, load => load_bitmap(10), clk => clk, o => output0a);
	reg11: reg32 port map(d => reg_input, load => load_bitmap(11), clk => clk, o => output0b);
	reg12: reg32 port map(d => reg_input, load => load_bitmap(12), clk => clk, o => output0c);
	reg13: reg32 port map(d => reg_input, load => load_bitmap(13), clk => clk, o => output0d);
	reg14: reg32 port map(d => reg_input, load => load_bitmap(14), clk => clk, o => output0e);
	reg15: reg32 port map(d => reg_input, load => load_bitmap(15), clk => clk, o => output0f);
	reg16: reg32 port map(d => reg_input, load => load_bitmap(16), clk => clk, o => output10);
	reg17: reg32 port map(d => reg_input, load => load_bitmap(17), clk => clk, o => output11);
	reg18: reg32 port map(d => reg_input, load => load_bitmap(18), clk => clk, o => output12);
	reg19: reg32 port map(d => reg_input, load => load_bitmap(19), clk => clk, o => output13);
	reg20: reg32 port map(d => reg_input, load => load_bitmap(20), clk => clk, o => output14);
	reg21: reg32 port map(d => reg_input, load => load_bitmap(21), clk => clk, o => output15);
	reg22: reg32 port map(d => reg_input, load => load_bitmap(22), clk => clk, o => output16);
	reg23: reg32 port map(d => reg_input, load => load_bitmap(23), clk => clk, o => output17);
	reg24: reg32 port map(d => reg_input, load => load_bitmap(24), clk => clk, o => output18);
	reg25: reg32 port map(d => reg_input, load => load_bitmap(25), clk => clk, o => output19);
	reg26: reg32 port map(d => reg_input, load => load_bitmap(26), clk => clk, o => output1a);
	reg27: reg32 port map(d => reg_input, load => load_bitmap(27), clk => clk, o => output1b);
	reg28: reg32 port map(d => reg_input, load => load_bitmap(28), clk => clk, o => output1c);
	reg29: reg32 port map(d => reg_input, load => load_bitmap(29), clk => clk, o => output1d);
	reg30: reg32 port map(d => reg_input, load => load_bitmap(30), clk => clk, o => output1e);
	reg31: reg32 port map(d => reg_input, load => load_bitmap(31), clk => clk, o => output1f);
	data_mux: mux32_32 port map(
		s => src_reg,
		in00 => output00, in01 => output01, in02 => output02, in03 => output03,
		in04 => output04, in05 => output05, in06 => output06, in07 => output07,
		in08 => output08, in09 => output09, in0a => output0a, in0b => output0b,
		in0c => output0c, in0d => output0d, in0e => output0e, in0f => output0f,
		in10 => output10, in11 => output11, in12 => output12, in13 => output13,
		in14 => output14, in15 => output15, in16 => output16, in17 => output17,
		in18 => output18, in19 => output19, in1a => output1a, in1b => output1b,
		in1c => output1c, in1d => output1d, in1e => output1e, in1f => output1f,
		o => loopback);

end Behavioral;

