library ieee;
use ieee.std_logic_1164.all;

entity decoder_5to32 is
	port(
		s : in  std_logic_vector(4 downto 0);
		o : out std_logic_vector(31 downto 0));
end decoder_5to32;

architecture Behavioral of decoder_5to32 is
begin
	process(s)
	begin
		o(0) <= ((not s(0)) and (not s(1)) and (not s(2)) and (not s(3)) and (not s(4))) after 1 ns;
		o(1) <= (s(0)       and (not s(1)) and (not s(2)) and (not s(3)) and (not s(4))) after 1 ns;
		o(2) <= ((not s(0)) and s(1)       and (not s(2)) and (not s(3)) and (not s(4))) after 1 ns;
		o(3) <= (s(0)       and s(1)       and (not s(2)) and (not s(3)) and (not s(4))) after 1 ns;
		o(4) <= ((not s(0)) and (not s(1)) and s(2)       and (not s(3)) and (not s(4))) after 1 ns;
		o(5) <= (s(0)       and (not s(1)) and s(2)       and (not s(3)) and (not s(4))) after 1 ns;
		o(6) <= ((not s(0)) and s(1)       and s(2)       and (not s(3)) and (not s(4))) after 1 ns;
		o(7) <= (s(0)       and s(1)       and s(2)       and (not s(3)) and (not s(4))) after 1 ns;
		o(8) <= ((not s(0)) and (not s(1)) and (not s(2)) and s(3)       and (not s(4))) after 1 ns;
		o(9) <= (s(0)       and (not s(1)) and (not s(2)) and s(3)       and (not s(4))) after 1 ns;
		o(10) <= ((not s(0)) and s(1)       and (not s(2)) and s(3)       and (not s(4))) after 1 ns;
		o(11) <= (s(0)       and s(1)       and (not s(2)) and s(3)       and (not s(4))) after 1 ns;
		o(12) <= ((not s(0)) and (not s(1)) and s(2)       and s(3)       and (not s(4))) after 1 ns;
		o(13) <= (s(0)       and (not s(1)) and s(2)       and s(3)       and (not s(4))) after 1 ns;
		o(14) <= ((not s(0)) and s(1)       and s(2)       and s(3)       and (not s(4))) after 1 ns;
		o(15) <= (s(0)       and s(1)       and s(2)       and s(3)       and (not s(4))) after 1 ns;
		o(16) <= ((not s(0)) and (not s(1)) and (not s(2)) and (not s(3)) and s(4)      ) after 1 ns;
		o(17) <= (s(0)       and (not s(1)) and (not s(2)) and (not s(3)) and s(4)      ) after 1 ns;
		o(18) <= ((not s(0)) and s(1)       and (not s(2)) and (not s(3)) and s(4)      ) after 1 ns;
		o(19) <= (s(0)       and s(1)       and (not s(2)) and (not s(3)) and s(4)      ) after 1 ns;
		o(20) <= ((not s(0)) and (not s(1)) and s(2)       and (not s(3)) and s(4)      ) after 1 ns;
		o(21) <= (s(0)       and (not s(1)) and s(2)       and (not s(3)) and s(4)      ) after 1 ns;
		o(22) <= ((not s(0)) and s(1)       and s(2)       and (not s(3)) and s(4)      ) after 1 ns;
		o(23) <= (s(0)       and s(1)       and s(2)       and (not s(3)) and s(4)      ) after 1 ns;
		o(24) <= ((not s(0)) and (not s(1)) and (not s(2)) and s(3)       and s(4)      ) after 1 ns;
		o(25) <= (s(0)       and (not s(1)) and (not s(2)) and s(3)       and s(4)      ) after 1 ns;
		o(26) <= ((not s(0)) and s(1)       and (not s(2)) and s(3)       and s(4)      ) after 1 ns;
		o(27) <= (s(0)       and s(1)       and (not s(2)) and s(3)       and s(4)      ) after 1 ns;
		o(28) <= ((not s(0)) and (not s(1)) and s(2)       and s(3)       and s(4)      ) after 1 ns;
		o(29) <= (s(0)       and (not s(1)) and s(2)       and s(3)       and s(4)      ) after 1 ns;
		o(30) <= ((not s(0)) and s(1)       and s(2)       and s(3)       and s(4)      ) after 1 ns;
		o(31) <= (s(0)       and s(1)       and s(2)       and s(3)       and s(4)      ) after 1 ns;
	end process;
end Behavioral;

