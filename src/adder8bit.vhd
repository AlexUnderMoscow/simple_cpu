library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity adder8bit is
port(
	a: 		in std_logic_vector(7 downto 0);
	b: 		in std_logic_vector(7 downto 0);
	sum: 		out std_logic_vector(7 downto 0);
	ci: 		in std_logic;
	co: 		out std_logic

	);
end adder8bit;

architecture Behavioral of adder8bit is

component fulladder is
    Port (
	a: 		in std_logic;
	b: 		in std_logic; 
	ci:		in std_logic;
	res: 		out std_logic;
	co:		out std_logic
	 );
end component;

signal c1,c2,c3,c4,c5,c6,c7	: std_logic;

begin
s0 :fulladder port map(a=>a(0),b=>b(0),ci=>ci,res=>sum(0),co=>c1);
s1 :fulladder port map(a=>a(1),b=>b(1),ci=>c1,res=>sum(1),co=>c2);
s2 :fulladder port map(a=>a(2),b=>b(2),ci=>c2,res=>sum(2),co=>c3);
s3 :fulladder port map(a=>a(3),b=>b(3),ci=>c3,res=>sum(3),co=>c4);
s4 :fulladder port map(a=>a(4),b=>b(4),ci=>c4,res=>sum(4),co=>c5);
s5 :fulladder port map(a=>a(5),b=>b(5),ci=>c5,res=>sum(5),co=>c6);
s6 :fulladder port map(a=>a(6),b=>b(6),ci=>c6,res=>sum(6),co=>c7);
s7 :fulladder port map(a=>a(7),b=>b(7),ci=>c7,res=>sum(7),co=>co);
end Behavioral;