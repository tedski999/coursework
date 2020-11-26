library ieee;
use ieee.std_logic_1164.all;

entity decoder_5to32 is
	port(
		input  : in  std_logic_vector(4 downto 0);
		output : out std_logic_vector(31 downto 0));
end decoder_5to32;

architecture Behavioral of decoder_5to32 is
begin

	output(0)  <= ((not input(0)) and (not input(1)) and (not input(2)) and (not input(3)) and (not input(4))) after 5 ns;
	output(1)  <= (input(0)       and (not input(1)) and (not input(2)) and (not input(3)) and (not input(4))) after 5 ns;
	output(2)  <= ((not input(0)) and input(1)       and (not input(2)) and (not input(3)) and (not input(4))) after 5 ns;
	output(3)  <= (input(0)       and input(1)       and (not input(2)) and (not input(3)) and (not input(4))) after 5 ns;
	output(4)  <= ((not input(0)) and (not input(1)) and input(2)       and (not input(3)) and (not input(4))) after 5 ns;
	output(5)  <= (input(0)       and (not input(1)) and input(2)       and (not input(3)) and (not input(4))) after 5 ns;
	output(6)  <= ((not input(0)) and input(1)       and input(2)       and (not input(3)) and (not input(4))) after 5 ns;
	output(7)  <= (input(0)       and input(1)       and input(2)       and (not input(3)) and (not input(4))) after 5 ns;
	output(8)  <= ((not input(0)) and (not input(1)) and (not input(2)) and input(3)       and (not input(4))) after 5 ns;
	output(9)  <= (input(0)       and (not input(1)) and (not input(2)) and input(3)       and (not input(4))) after 5 ns;
	output(10) <= ((not input(0)) and input(1)       and (not input(2)) and input(3)       and (not input(4))) after 5 ns;
	output(11) <= (input(0)       and input(1)       and (not input(2)) and input(3)       and (not input(4))) after 5 ns;
	output(12) <= ((not input(0)) and (not input(1)) and input(2)       and input(3)       and (not input(4))) after 5 ns;
	output(13) <= (input(0)       and (not input(1)) and input(2)       and input(3)       and (not input(4))) after 5 ns;
	output(14) <= ((not input(0)) and input(1)       and input(2)       and input(3)       and (not input(4))) after 5 ns;
	output(15) <= (input(0)       and input(1)       and input(2)       and input(3)       and (not input(4))) after 5 ns;
	output(16) <= ((not input(0)) and (not input(1)) and (not input(2)) and (not input(3)) and input(4)      ) after 5 ns;
	output(17) <= (input(0)       and (not input(1)) and (not input(2)) and (not input(3)) and input(4)      ) after 5 ns;
	output(18) <= ((not input(0)) and input(1)       and (not input(2)) and (not input(3)) and input(4)      ) after 5 ns;
	output(19) <= (input(0)       and input(1)       and (not input(2)) and (not input(3)) and input(4)      ) after 5 ns;
	output(20) <= ((not input(0)) and (not input(1)) and input(2)       and (not input(3)) and input(4)      ) after 5 ns;
	output(21) <= (input(0)       and (not input(1)) and input(2)       and (not input(3)) and input(4)      ) after 5 ns;
	output(22) <= ((not input(0)) and input(1)       and input(2)       and (not input(3)) and input(4)      ) after 5 ns;
	output(23) <= (input(0)       and input(1)       and input(2)       and (not input(3)) and input(4)      ) after 5 ns;
	output(24) <= ((not input(0)) and (not input(1)) and (not input(2)) and input(3)       and input(4)      ) after 5 ns;
	output(25) <= (input(0)       and (not input(1)) and (not input(2)) and input(3)       and input(4)      ) after 5 ns;
	output(26) <= ((not input(0)) and input(1)       and (not input(2)) and input(3)       and input(4)      ) after 5 ns;
	output(27) <= (input(0)       and input(1)       and (not input(2)) and input(3)       and input(4)      ) after 5 ns;
	output(28) <= ((not input(0)) and (not input(1)) and input(2)       and input(3)       and input(4)      ) after 5 ns;
	output(29) <= (input(0)       and (not input(1)) and input(2)       and input(3)       and input(4)      ) after 5 ns;
	output(30) <= ((not input(0)) and input(1)       and input(2)       and input(3)       and input(4)      ) after 5 ns;
	output(31) <= (input(0)       and input(1)       and input(2)       and input(3)       and input(4)      ) after 5 ns;

end Behavioral;

