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
		o(0) <= ((not s(0)) and (not s(1)) and (not s(2)) and (not s(3)) and (not s(4))) after 5 ns;
		o(1) <= (s(0)       and (not s(1)) and (not s(2)) and (not s(3)) and (not s(4))) after 5 ns;
		o(2) <= ((not s(0)) and s(1)       and (not s(2)) and (not s(3)) and (not s(4))) after 5 ns;
		o(3) <= (s(0)       and s(1)       and (not s(2)) and (not s(3)) and (not s(4))) after 5 ns;
		o(4) <= ((not s(0)) and (not s(1)) and s(2)       and (not s(3)) and (not s(4))) after 5 ns;
		o(5) <= (s(0)       and (not s(1)) and s(2)       and (not s(3)) and (not s(4))) after 5 ns;
		o(6) <= ((not s(0)) and s(1)       and s(2)       and (not s(3)) and (not s(4))) after 5 ns;
		o(7) <= (s(0)       and s(1)       and s(2)       and (not s(3)) and (not s(4))) after 5 ns;
		o(0) <= ((not s(0)) and (not s(1)) and (not s(2)) and s(3)       and (not s(4))) after 5 ns;
		o(1) <= (s(0)       and (not s(1)) and (not s(2)) and s(3)       and (not s(4))) after 5 ns;
		o(2) <= ((not s(0)) and s(1)       and (not s(2)) and s(3)       and (not s(4))) after 5 ns;
		o(3) <= (s(0)       and s(1)       and (not s(2)) and s(3)       and (not s(4))) after 5 ns;
		o(4) <= ((not s(0)) and (not s(1)) and s(2)       and s(3)       and (not s(4))) after 5 ns;
		o(5) <= (s(0)       and (not s(1)) and s(2)       and s(3)       and (not s(4))) after 5 ns;
		o(6) <= ((not s(0)) and s(1)       and s(2)       and s(3)       and (not s(4))) after 5 ns;
		o(7) <= (s(0)       and s(1)       and s(2)       and s(3)       and (not s(4))) after 5 ns;
		o(0) <= ((not s(0)) and (not s(1)) and (not s(2)) and (not s(3)) and s(4)      ) after 5 ns;
		o(1) <= (s(0)       and (not s(1)) and (not s(2)) and (not s(3)) and s(4)      ) after 5 ns;
		o(2) <= ((not s(0)) and s(1)       and (not s(2)) and (not s(3)) and s(4)      ) after 5 ns;
		o(3) <= (s(0)       and s(1)       and (not s(2)) and (not s(3)) and s(4)      ) after 5 ns;
		o(4) <= ((not s(0)) and (not s(1)) and s(2)       and (not s(3)) and s(4)      ) after 5 ns;
		o(5) <= (s(0)       and (not s(1)) and s(2)       and (not s(3)) and s(4)      ) after 5 ns;
		o(6) <= ((not s(0)) and s(1)       and s(2)       and (not s(3)) and s(4)      ) after 5 ns;
		o(7) <= (s(0)       and s(1)       and s(2)       and (not s(3)) and s(4)      ) after 5 ns;
		o(0) <= ((not s(0)) and (not s(1)) and (not s(2)) and s(3)       and s(4)      ) after 5 ns;
		o(1) <= (s(0)       and (not s(1)) and (not s(2)) and s(3)       and s(4)      ) after 5 ns;
		o(2) <= ((not s(0)) and s(1)       and (not s(2)) and s(3)       and s(4)      ) after 5 ns;
		o(3) <= (s(0)       and s(1)       and (not s(2)) and s(3)       and s(4)      ) after 5 ns;
		o(4) <= ((not s(0)) and (not s(1)) and s(2)       and s(3)       and s(4)      ) after 5 ns;
		o(5) <= (s(0)       and (not s(1)) and s(2)       and s(3)       and s(4)      ) after 5 ns;
		o(6) <= ((not s(0)) and s(1)       and s(2)       and s(3)       and s(4)      ) after 5 ns;
		o(7) <= (s(0)       and s(1)       and s(2)       and s(3)       and s(4)      ) after 5 ns;
	end process;
end Behavioral;

