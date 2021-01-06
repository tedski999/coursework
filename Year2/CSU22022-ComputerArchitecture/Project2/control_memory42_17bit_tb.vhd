library ieee;
use ieee.std_logic_1164.all;

entity control_memory42_17bit_tb is
end control_memory42_17bit_tb;

architecture Behavioral of control_memory42_17bit_tb is

	component control_memory42_17bit is
		port (
			input_addr     : in std_logic_vector(16 downto 0);
			mc, il, pi, pl : out std_logic;                      -- ports 21-18
			td, ta, tb, mb : out std_logic;                      -- ports 17-14
			md, rw, mm, mw : out std_logic;                      -- ports 8-5
			rv, rc, rn, rz : out std_logic;                      -- ports 4-1
			fl             : out std_logic;                      -- port 0
			fs             : out std_logic_vector(4 downto 0);   -- ports 13 downto 9
			ms             : out std_logic_vector(2 downto 0);   -- ports 24 downto 22
			na             : out std_logic_vector(16 downto 0)); -- ports 41 downto 25
	end component;

	signal input_addr     : std_logic_vector(16 downto 0);
	signal mc, il, pi, pl : std_logic;
	signal td, ta, tb, mb : std_logic;
	signal md, rw, mm, mw : std_logic;
	signal rv, rc, rn, rz : std_logic;
	signal fl             : std_logic;
	signal fs             : std_logic_vector(4 downto 0);
	signal ms             : std_logic_vector(2 downto 0);
	signal na             : std_logic_vector(16 downto 0);

begin

	uut: control_memory42_17bit port map(
		input_addr => input_addr,
		mc => mc, il => il, pi => pi, pl => pl,
		td => td, ta => ta, tb => tb, mb => mb,
		md => md, rw => rw, mm => mm, mw => mw,
		rv => rv, rc => rc, rn => rn, rz => rz,
		fl => fl, fs => fs, ms => ms, na => na);

	stim_proc: process
	begin

		-- Test #1 - Read from 0x00
		input_addr <= "00000000000000000"; wait for 25 ns;
		assert (na&ms&mc&il&pi&pl&td&ta&tb&mb&fs&md&rw&mm&mw&rv&rc&rn&rz&fl) = "000000000000000000010000000011111000000000" report "control_memory42_17bit test #1" severity failure;

		-- Test #2 - Read from 0x05
		input_addr <= "00000000000000101"; wait for 25 ns;
		assert (na&ms&mc&il&pi&pl&td&ta&tb&mb&fs&md&rw&mm&mw&rv&rc&rn&rz&fl) = "000000000000000010010000000000000110000000" report "control_memory42_17bit test #2" severity failure;

		std.env.finish;

	end process;

end Behavioral;

