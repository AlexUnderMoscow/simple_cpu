library ieee;
use ieee.std_logic_1164.all;
entity fulladder is
port(
	a: 		in std_logic;
	b: 		in std_logic; 
	ci:		in std_logic;
	res: 		out std_logic;
	co:		out std_logic
	);
end fulladder;

architecture Behavioral of fulladder is

begin
	res <= ci xor (a xor b);
	co <= (ci and (a xor b)) or (a and b);
end Behavioral;