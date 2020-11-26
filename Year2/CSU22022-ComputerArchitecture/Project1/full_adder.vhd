library ieee;
use ieee.std_logic_1164.all;

entity full_adder is
	port(
		a, b, carry_in : in std_logic;
		sum, carry_out : out std_logic);
end full_adder;

architecture Behavioral of full_adder is
	signal intermediate : std_logic; -- Common signal between sum and carry_out
begin
	-- Maximum gate delay: 15 ns
	intermediate <= a xor b after 5 ns;
	sum <= intermediate xor carry_in after 5 ns;
	carry_out <= (intermediate and carry_in) or (a and b) after 10 ns;
end Behavioral;

