

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;

entity PC is
port(
	d_in: 	in std_logic_vector(4 downto 0);
	reset: 	in std_logic;
	en: 		in std_logic;	
	clk:		in std_logic;
	d_out: 	out std_logic_vector(4 downto 0)
	);
end PC;

architecture Behavioral of PC is

begin
 process(clk, reset)
   begin
		if (reset='1') then
			d_out<="00000";
      elsif (rising_edge(clk)) then
			if en='1' then
				d_out<=d_in;
			end if;
		end if;
   end process; 

end Behavioral;

--------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;

entity SP is
port(
		reset: 	in std_logic; 
		action: 	in std_logic; 
		push: 	in std_logic; 
		clk:		in std_logic;
		d_out: 	out std_logic_vector(4 downto 0)
	);
end SP;

architecture Behavioral of SP is
signal addr: std_logic_vector(4 downto 0);
begin
	d_out<=addr;
 process(clk, reset)
   begin
		if (reset='1') then
			addr<="11111";
      elsif (rising_edge(clk)) then
			if (action = '1') then
				if (push='0') then
					addr<=addr+1;
				else
					addr<=addr-1;
				end if;
			end if;
		end if;
   end process; 

end Behavioral;
--------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity AC is
port(
	d_in: 	in std_logic_vector(7 downto 0);
	load: 	in std_logic;
	reset: 	in std_logic; 
	clk:		in std_logic;
	d_out: 	out std_logic_vector(7 downto 0)
	);
end AC;

architecture Behavioral of AC is

begin
 process(clk, reset)
   begin
		if (reset='1') then
			d_out<="00000000";
      elsif (rising_edge(clk)) then
				if (load = '1') then
					d_out <= d_in;
				end if;
		end if;
   end process; 

end Behavioral;

----
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity ALU is
port(
	a: 	in std_logic_vector(7 downto 0);
	b: 	in std_logic_vector(7 downto 0);
	sel:	in std_logic;
	alu_out: 	out std_logic_vector(7 downto 0)
	);
end ALU;

architecture Behavioral of ALU is
--!!!! - not working
component adder8bit is
port(
	a: 		in std_logic_vector(7 downto 0);
	b: 		in std_logic_vector(7 downto 0);
	sum: 		out std_logic_vector(7 downto 0);
	ci: 		in std_logic;
	co: 		out std_logic
	);
end component;

signal invertB: 	std_logic_vector(7 downto 0);
signal plus1: 		std_logic_vector(7 downto 0);
signal tmp: 		std_logic_vector(7 downto 0);
signal zero: 		std_logic:='0';
signal one: 		std_logic_vector(7 downto 0):="00000001";

begin

invertB(0) <= not b(0);
invertB(1) <= not b(1);
invertB(2) <= not b(2);
invertB(3) <= not b(3);
invertB(4) <= not b(4);
invertB(5) <= not b(5);
invertB(6) <= not b(6);
invertB(7) <= not b(7);

add1 :adder8bit
port map(
	a=>one,
	b(0)=>invertB(0),
	b(1)=>invertB(1),
	b(2)=>invertB(2),
	b(3)=>invertB(3),
	b(4)=>invertB(4),
	b(5)=>invertB(5),
	b(6)=>invertB(6),
	b(7)=>invertB(7),
	sum=> plus1,
	ci=> zero,
	co=>open
);

add :adder8bit
port map(
	a=>a,
	b(0)=>tmp(0),
	b(1)=>tmp(1),
	b(2)=>tmp(2),
	b(3)=>tmp(3),
	b(4)=>tmp(4),
	b(5)=>tmp(5),
	b(6)=>tmp(6),
	b(7)=>tmp(7),
	sum=> alu_out,
	ci=> zero,
	co=>open
);

 process(plus1, b, sel)
   begin
	if (sel='0') then
			tmp <= b;	
		else
			tmp <=plus1;
	end if;
   end process; 

end Behavioral;

------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity ADDER is
port(
	a: 	in std_logic_vector(4 downto 0);
	b: 	in std_logic_vector(4 downto 0);
	adder_out: 	out std_logic_vector(4 downto 0)
	);
end ADDER;

architecture Behavioral of ADDER is

begin
 process(a, b)
   begin
			adder_out<=a+b;
   end process; 

end Behavioral;

----------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity MUX2TO1_6B is
port(
	i0: 	in std_logic_vector(4 downto 0);
	i1: 	in std_logic_vector(4 downto 0);
	sel: 	in std_logic;
	mux_out: 	out std_logic_vector(4 downto 0)
	);
end MUX2TO1_6B;

architecture Behavioral of MUX2TO1_6B is

begin
 mux_out <= i1 when (sel = '1') else i0;

end Behavioral;

----------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity MUX2TO1_8B is
port(
	i0: 	in std_logic_vector(7 downto 0);
	i1: 	in std_logic_vector(7 downto 0);
	sel: 	in std_logic;
	mux_out: 	out std_logic_vector(7 downto 0)
	);
end MUX2TO1_8B;

architecture Behavioral of MUX2TO1_8B is

begin
 mux_out <= i1 when (sel = '1') else i0;

end Behavioral;

----------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity DataPath is
port(
	 reset: 			in std_logic;
	 ld_ac: 			in std_logic;
	 ac_src: 		in std_logic;
	 sp_action: 	in std_logic;
	 sp_push: 		in std_logic;
	 sp_addr: 		in std_logic;
	 alu_sel: 		in std_logic;
	 pc_src: 		in std_logic;
	 halt: 			in std_logic;
	 ac_mov:			in std_logic;
	 result_flag: 	in std_logic;
	 pc_en: 			in std_logic;	
	 clk: 			in std_logic;
	 flags:			out std_logic_vector(2 downto 0);
	opcode:			out std_logic_vector(2 downto 0);
	operand:			out std_logic_vector(4 downto 0);
	im_abus:			out std_logic_vector(4 downto 0); --instruction addr bus
	im_dbus: 		in std_logic_vector(7 downto 0); --instruction data bus
	
	dm_abus:				out std_logic_vector(4 downto 0); --data mem adr bus
	dm_in_dbus: 		in std_logic_vector(7 downto 0);--data mem input data bus
	dm_out_dbus :		out std_logic_vector(7 downto 0)		--data mem output data bus
	);
end DataPath;

architecture Behavioral of DataPath is

component PC is
    Port (
	d_in: 	in std_logic_vector(4 downto 0);
	reset: 	in std_logic;
	en: 		in std_logic;		
	clk:		in std_logic;
	d_out: 	out std_logic_vector(4 downto 0)
	 );
end component;

component SP is
    Port (
		reset: 	in std_logic; 
		action: 	in std_logic; 
		push: 	in std_logic; 
		clk:		in std_logic;
		d_out: 	out std_logic_vector(4 downto 0)
	 );
end component;

component DF is
   port
   (
      clk : in std_logic;
      rst : in std_logic;
      pre : in std_logic;
      ce  : in std_logic;
      d : in std_logic;
      q : out std_logic
   );
end component;

component AC is
port(
	d_in: 	in std_logic_vector(7 downto 0);
	load: 	in std_logic;
	reset: 	in std_logic; 
	clk:		in std_logic;
	d_out: 	out std_logic_vector(7 downto 0)
	);
end component;

component ALU is
port(
	a: 	in std_logic_vector(7 downto 0);
	b: 	in std_logic_vector(7 downto 0);
	sel:	in std_logic;
	alu_out: 	out std_logic_vector(7 downto 0)
	);
end component;

component ADDER is
port(
	a: 	in std_logic_vector(4 downto 0);
	b: 	in std_logic_vector(4 downto 0);
	adder_out: 	out std_logic_vector(4 downto 0)
	);
end component;

component MUX2TO1_6B is
port(
	i0: 	in std_logic_vector(4 downto 0);
	i1: 	in std_logic_vector(4 downto 0);
	sel: 	in std_logic;
	mux_out: 	out std_logic_vector(4 downto 0)
	);
end component;

component MUX2TO1_8B is
port(
	i0: 	in std_logic_vector(7 downto 0);
	i1: 	in std_logic_vector(7 downto 0);
	sel: 	in std_logic;
	mux_out: 	out std_logic_vector(7 downto 0)
	);
end component;


signal ac_out				: std_logic_vector(7 downto 0);
signal alu_out				: std_logic_vector(7 downto 0);
signal mux2_out			: std_logic_vector(7 downto 0);
signal pc_out				: std_logic_vector(4 downto 0);
signal sp_out				: std_logic_vector(4 downto 0);
signal adder_out			: std_logic_vector(4 downto 0);
signal mux1_out			: std_logic_vector(4 downto 0);
signal odin					: std_logic_vector(4 downto 0):="00001";
signal zero					: std_logic_vector(7 downto 0):="00000000";
signal result_zero		: std_logic;
signal stopClk 			: std_logic;
signal systemClk			: std_logic;
signal dataMov				: std_logic_vector(7 downto 0);
signal muxMovOpcodeOut	: std_logic_vector(7 downto 0);

begin

result_zero <= not( alu_out(7) or alu_out(6) or alu_out(6) or alu_out(4) or
alu_out(3) or alu_out(2) or  alu_out(1) or alu_out(0));

systemClk <= (not stopClk) and clk;

pc1 :PC
port map(
 d_in=>mux1_out,
	reset=>reset,
	en=>pc_en,
	clk=>systemClk,
	d_out=>pc_out
);

sp1 :SP
port map(
		reset=>reset,
		action=>sp_action,
		push=>sp_push,
		clk=>systemClk,
		d_out=>sp_out
);

dataAddrMux : MUX2TO1_6B 
port map(im_dbus(4 downto 0), sp_out, sp_addr, dm_abus); 	

zFlag: DF
port map(
      clk => systemClk,
      rst =>reset,
      pre =>odin(4),
      ce  =>result_flag,
      d =>result_zero,
      q => flags(0) --zero
);

nFlag: DF
port map(
      clk => systemClk,
      rst =>reset,
      pre =>odin(4),
      ce  =>result_flag,
      d =>alu_out(7),
      q => flags(1) --negative
);

hFlag: DF
port map(
      clk => clk,
      rst =>reset,
      pre =>odin(4),
      ce  =>halt,
      d =>odin(0),
      q => stopClk --stop clk
);

ac1 :AC
port map(mux2_out, ld_ac, reset, systemClk, ac_out);

alu1 :ALU
port map(
	a=>ac_out,
	sel => alu_sel,
	b(7 downto 0)=>dm_in_dbus,
	alu_out => alu_out
);

adder1: ADDER 
port map(pc_out, odin, adder_out);

mux1: MUX2TO1_6B 
port map(adder_out, im_dbus(4 downto 0), pc_src, mux1_out);
 
mux2 : MUX2TO1_8B 
port map(alu_out, dataMov, ac_src, mux2_out); 	
--------
muxMovData : MUX2TO1_8B 
port map(dm_in_dbus, im_dbus, ac_mov, dataMov); 	

muxMovOpcode : MUX2TO1_8B 
port map(im_dbus, zero, ac_mov, muxMovOpcodeOut); 	
---------
 opcode <= muxMovOpcodeOut(7 downto 5);
 im_abus <= pc_out;
 dm_out_dbus <= ac_out;
 operand <= muxMovOpcodeOut (4 downto 0);

end Behavioral;

----------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity Controller is
port(
	clk:				in std_logic;
	reset:			in std_logic;
	opcode: 			in std_logic_vector(2 downto 0);
	operand:			in std_logic_vector(4 downto 0);
	flags:			in std_logic_vector(2 downto 0);
	rd_mem: 			out std_logic;
	result_flag: 	out std_logic;
	wr_mem: 			out std_logic;
	ac_src: 			out std_logic;
	alu_sel: 		out std_logic;
	ld_ac: 			out std_logic;
	ac_mov:			out std_logic;
	halt: 			out std_logic;
	pc_en: 			out std_logic;	
	sp_action: 		out std_logic;
	sp_push: 		out std_logic;
	sp_addr: 		out std_logic;
	pc_src: 			out std_logic
	);
end Controller;

architecture Behavioral of Controller is

TYPE State_type IS (Decode, Push, Pop, Mov);  	-- Define the states
	SIGNAL State : State_Type:=Decode;    			-- Create a signal that uses 
	signal wrieAssist: std_logic;
	signal writeMem: std_logic;
	signal operation: std_logic_vector(7 downto 0);
	signal ld_ac_allow: std_logic;
	signal ld_ac_assist: std_logic;
	signal ac_src_assist: std_logic;
begin
wr_mem <= wrieAssist and writeMem;

ac_src<=ac_src_assist or ld_ac_allow;
ld_ac <= ld_ac_assist or ld_ac_allow;

operation(7 downto 5) <= opcode;
operation(4 downto 0) <= operand;
PROCESS (clk, reset)
Begin
	if (reset='1') then
		State<=Decode;
		pc_en<='1';
		sp_action<='0';		
		sp_push<='0';
		wrieAssist<='1';
		ld_ac_allow<='0';
		ac_mov<='0';
	elsif rising_edge(clk) then 
		case State is
		WHEN Decode => 
			IF (opcode="000" and operand="00001") THEN --push
				pc_en<='0';
				wrieAssist<='0';
				sp_action<='1';		
				sp_push<='1';
				ac_mov<='0';
				ld_ac_allow<='0';
				State <= Push; 
			elsif (opcode="000" and operand="00010") THEN --pop
				pc_en<='0';
				wrieAssist<='0';
				sp_action<='1';		
				sp_push<='0';
				ac_mov<='0';
				ld_ac_allow<='0';
				State <= Pop; 
			elsif (opcode="000" and operand="00011") THEN --mov
				pc_en<='1';
				wrieAssist<='1';
				sp_action<='0';		
				sp_push<='0';
				ac_mov<='1';
				ld_ac_allow<='1';
				State <= Mov; 
			else
				pc_en<='1';
				ac_mov<='0';
				wrieAssist<='1';
				sp_action<='0';		
				sp_push<='0';
				State <= Decode;
			END IF;  			
		WHEN Push =>
				pc_en<='1';
				wrieAssist<='1';
				sp_action<='0';		
				sp_push<='0';
				ac_mov<='0';
				ld_ac_allow<='0';
				State <= Decode; 
		WHEN Pop =>
				pc_en<='1';
				wrieAssist<='1';
				sp_action<='0';		
				sp_push<='0';
				ac_mov<='0';
				ld_ac_allow<='0';
				State <= Decode; 
		WHEN Mov=> 
				pc_en<='1';
				wrieAssist<='1';
				sp_action<='0';		
				sp_push<='0';
				ac_mov<='0';
				ld_ac_allow<='0';
				State <= Decode; 
		WHEN others =>
			pc_en<='1';
			wrieAssist<='1';
			sp_action<='0';		
			sp_push<='0';
			ac_mov<='0';
			ld_ac_allow<='0';
			State <= Decode;		
		end case;
	end if;
end process;

 process(operation)
   begin
		case operation is
  when "00011111" =>   				
												--halt
			rd_mem <= '0';  				--xxx
			halt <= '1';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';

	
	when "00000000" =>
												--nop
			rd_mem <= '0';  				--xxx
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

	
	when "00000001" =>		
												--push
			rd_mem <= '0';  				--
			halt <= '0';
			result_flag <= '0';
			if (State=Push) then
				writeMem <= '0';
				sp_addr<='0';
			else
				writeMem <= '1';
				sp_addr<='1';
			end if;
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';

		
	when "00000010" =>	
													--pop
			writeMem <= '0';  				--
			halt <= '0';
			result_flag <= '0';
			if (State=Pop) then
				rd_mem <= '0';
				ld_ac_assist <= '0';
				sp_addr<='0';
			else
				rd_mem <= '1';
				sp_addr<='1';
				ld_ac_assist <= '1';
			end if;
			ac_src_assist <= '1';
			alu_sel <= '0';
			pc_src <= '0';

			
	when "00000011" =>	
													--mov	
		if (State=Mov) then
		--second action
			rd_mem <= '0';  				--xxx
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

		else
		--first action
			rd_mem <= '0';  				--xxx
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

		end if;

  when 		x"20" | x"21"  | x"22" | x"23" | x"24" | x"25" | x"26" | x"27"
			| 	x"28" | x"29"  | x"2A" | x"2B" | x"2C" | x"2D" | x"2E" | x"2F"
			|	x"30" | x"31"  | x"32" | x"33" | x"34" | x"35" | x"36" | x"37"
			| 	x"38" | x"39"  | x"3A" | x"3B" | x"3C" | x"3D" | x"3E" | x"3F"
			=>   																--lda adr "001"
			rd_mem <= '1';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '1';
			ac_src_assist <= '1';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

	  when 	x"40" | x"41"  | x"42" | x"43" | x"44" | x"45" | x"46" | x"47"
			| 	x"48" | x"49"  | x"4A" | x"4B" | x"4C" | x"4D" | x"4E" | x"4F"
			|	x"50" | x"51"  | x"52" | x"53" | x"54" | x"55" | x"56" | x"57"
			| 	x"58" | x"59"  | x"5A" | x"5B" | x"5C" | x"5D" | x"5E" | x"5F"
			=>																	--add adr "010"
  			rd_mem <= '1';
			halt <= '0';
			result_flag <= '1';
			writeMem <= '0';		
			ld_ac_assist <= '1';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

	when 		x"60" | x"61"  | x"62" | x"63" | x"64" | x"65" | x"66" | x"67"
			| 	x"68" | x"69"  | x"6A" | x"6B" | x"6C" | x"6D" | x"6E" | x"6F"
			|	x"70" | x"71"  | x"72" | x"73" | x"74" | x"75" | x"76" | x"77"
			| 	x"78" | x"79"  | x"7A" | x"7B" | x"7C" | x"7D" | x"7E" | x"7F" 
			=>																	--dec adr "011"
  			rd_mem <= '1';
			halt <= '0';
			result_flag <= '1';
			writeMem <= '0';		
			ld_ac_assist <= '1';
			ac_src_assist <= '0';
			alu_sel <= '1';
			pc_src <= '0';
			sp_addr<='0';

  when 		x"80" | x"81"  | x"82" | x"83" | x"84" | x"85" | x"86" | x"87"
			| 	x"88" | x"89"  | x"8A" | x"8B" | x"8C" | x"8D" | x"8E" | x"8F"
			|	x"90" | x"91"  | x"92" | x"93" | x"94" | x"95" | x"96" | x"97"
			| 	x"98" | x"99"  | x"9A" | x"9B" | x"9C" | x"9D" | x"9E" | x"9F"
			=>																-- sta adr "100"
			rd_mem <= '0';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '1';		
			ld_ac_assist <= '0';  		
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
			sp_addr<='0';

  when 		x"A0" | x"A1"  | x"A2" | x"A3" | x"A4" | x"A5" | x"A6" | x"A7"
			| 	x"A8" | x"A9"  | x"AA" | x"AB" | x"AC" | x"AD" | x"AE" | x"AF"
			|	x"B0" | x"B1"  | x"B2" | x"B3" | x"B4" | x"B5" | x"B6" | x"B7"
			| 	x"B8" | x"B9"  | x"BA" | x"BB" | x"BC" | x"BD" | x"BE" | x"BF"
			=>   																--jmp adr "101"
			rd_mem <= '0';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '1';

			sp_addr<='0';
	  when 	x"C0" | x"C1"  | x"C2" | x"C3" | x"C4" | x"C5" | x"C6" | x"C7"
			| 	x"C8" | x"C9"  | x"CA" | x"CB" | x"CC" | x"CD" | x"CE" | x"CF"
			|	x"D0" | x"D1"  | x"D2" | x"D3" | x"D4" | x"D5" | x"D6" | x"D7"
			| 	x"D8" | x"D9"  | x"DA" | x"DB" | x"DC" | x"DD" | x"DE" | x"DF"
			=>   																--jz adr "110"

			sp_addr<='0';
	  if (flags(0)='1') then			--flag zero
			rd_mem <= '0';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '1';
		else
			rd_mem <= '0';  				--xxx
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
		end if;
		
		when 	x"E0" | x"E1"  | x"E2" | x"E3" | x"E4" | x"E5" | x"E6" | x"E7"
			| 	x"E8" | x"E9"  | x"EA" | x"EB" | x"EC" | x"ED" | x"EE" | x"EF"
			|	x"F0" | x"F1"  | x"F2" | x"F3" | x"F4" | x"F5" | x"F6" | x"F7"
			| 	x"F8" | x"F9"  | x"FA" | x"FB" | x"FC" | x"FD" | x"FE" | x"FF"
		=>   			--jn adr "111"

			sp_addr<='0';
	  if (flags(1)='1') then			--flag negative
			rd_mem <= '0';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '1';
		else
			rd_mem <= '0';  				--xxx
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
		end if;

 when others =>					--xxx
				
			sp_addr<='0';
			rd_mem <= '0';
			halt <= '0';
			result_flag <= '0';
			writeMem <= '0';		
			ld_ac_assist <= '0';
			ac_src_assist <= '0';
			alu_sel <= '0';
			pc_src <= '0';
end case;
   end process; 

end Behavioral;

----------------------------!!!!!!
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity smpl_vhdl is
port(
	 clk: 		in std_logic;
	 reset: 		in std_logic;
	 rd_mem: 	out std_logic;
	 wr_mem: 	out std_logic;
	 
	im_abus:		out std_logic_vector(4 downto 0);
	im_dbus:		in std_logic_vector(7 downto 0); 
	dm_abus: 	out std_logic_vector(4 downto 0);
	
	dm_in_dbus: in std_logic_vector(7 downto 0); 
	dm_out_dbus: out std_logic_vector(7 downto 0)

	);
end smpl_vhdl;

architecture Behavioral of smpl_vhdl is

component DataPath is
    Port (
	reset: 			in std_logic;
	 ld_ac: 			in std_logic;
	 ac_src: 		in std_logic;
	 alu_sel:		in std_logic;
	 pc_src: 		in std_logic;
	 result_flag: 	in std_logic;
	 sp_action: 	in std_logic;
	 sp_push: 		in std_logic;
	 sp_addr: 		in std_logic;
	 halt: 			in std_logic;
	 ac_mov:			in std_logic;
	 clk: 			in std_logic;
	 pc_en: 			in std_logic;	
	 flags:			out std_logic_vector(2 downto 0);
	 opcode:			out std_logic_vector(2 downto 0);
	 operand:		out std_logic_vector(4 downto 0);
	 im_abus:		out std_logic_vector(4 downto 0); --instruction addr bus
	 im_dbus: 		in std_logic_vector(7 downto 0); --instruction data bus
	
	dm_abus:				out std_logic_vector(4 downto 0); --data mem adr bus
	dm_in_dbus: 		in std_logic_vector(7 downto 0);--data mem input data bus
	dm_out_dbus :		out std_logic_vector(7 downto 0)		--data mem output data bus
	 );
end component;

component Controller is
port(
	clk:				in std_logic;
	reset:			in std_logic;
	opcode: 			in std_logic_vector(2 downto 0);
	operand:			in std_logic_vector(4 downto 0);
	rd_mem: 			out std_logic;
	flags:			in std_logic_vector(2 downto 0);
	wr_mem: 			out std_logic;
	result_flag: 	out std_logic;
	ac_src: 			out std_logic;
	alu_sel:			out std_logic;
	ld_ac: 			out std_logic;
	ac_mov:			out std_logic;
	pc_en: 			out std_logic;	
	halt: 			out std_logic;
	sp_action: 		out std_logic;
	sp_push: 		out std_logic;
	sp_addr: 		out std_logic;
	pc_src: 			out std_logic
	);
end component;

signal opcode			: std_logic_vector(2 downto 0);
signal ac_src			: std_logic;
signal ld_ac			: std_logic;
signal pc_src			: std_logic;
signal alu_sel			: std_logic;
signal halt				: std_logic;
signal result_flag	: std_logic;
signal sp_action		: std_logic;
signal ac_mov			: std_logic;
signal sp_push			: std_logic;
signal sp_addr			: std_logic;
signal pc_en			: std_logic;	
signal flags			: std_logic_vector(2 downto 0);
signal operand			: std_logic_vector(4 downto 0);

begin

dp1 :DataPath
port map(
	reset=>reset,
	ld_ac=>ld_ac,
	ac_src=>ac_src,
	result_flag=>result_flag,
	alu_sel => alu_sel,
	halt=>halt,
	pc_src=>pc_src,
	clk=>clk,
	flags=>flags,
	pc_en=>pc_en,
	ac_mov=>ac_mov,
	opcode=>opcode,
	operand => operand,
	im_abus=>im_abus,
	im_dbus=>im_dbus,
	sp_action=>sp_action,
	sp_push=>sp_push,
	sp_addr=>sp_addr,
	dm_abus=>dm_abus,
	dm_in_dbus=>dm_in_dbus,
	dm_out_dbus=>dm_out_dbus 	
);

c1 :Controller
port map(
	clk => clk,
	reset=>reset,
	opcode=>opcode,
	operand=>operand,
	rd_mem=>rd_mem,
	flags=>flags,
	wr_mem=>wr_mem,
	result_flag=>result_flag,
	alu_sel => alu_sel,
	ac_src=>ac_src,
	ac_mov=>ac_mov,
	halt=>halt,
	ld_ac=>ld_ac,
	pc_en=>pc_en,
	sp_action=>sp_action,
	sp_push=>sp_push,
	sp_addr=>sp_addr,
	pc_src=>pc_src
);

end Behavioral;