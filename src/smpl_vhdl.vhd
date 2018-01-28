

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;

entity PC is
port(
	d_in: 	in std_logic_vector(4 downto 0);
	reset: 	in std_logic; 
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
			d_out<=d_in;
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
	 alu_sel: 		in std_logic;
	 pc_src: 		in std_logic;
	 result_flag: 	in std_logic;
	 clk: 			in std_logic;
	 rst:				in std_logic;
	 flags:			out std_logic_vector(2 downto 0);
	opcode:			out std_logic_vector(2 downto 0);
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


signal ac_out			: std_logic_vector(7 downto 0);
signal alu_out			: std_logic_vector(7 downto 0);
signal mux2_out		: std_logic_vector(7 downto 0);
signal pc_out			: std_logic_vector(4 downto 0);
signal adder_out		: std_logic_vector(4 downto 0);
signal mux1_out		: std_logic_vector(4 downto 0);
signal odin				: std_logic_vector(4 downto 0):="00001";
signal result_zero	: std_logic;

begin

result_zero <= not( alu_out(7) or alu_out(6) or alu_out(6) or alu_out(4) or
alu_out(3) or alu_out(2) or  alu_out(1) or alu_out(0));

pc1 :PC
port map(
	d_in=>mux1_out,
	reset=>reset,
	clk=>clk,
	d_out=>pc_out
);

zFlag: DF
port map(
      clk => clk,
      rst =>rst,
      pre =>odin(4),
      ce  =>result_flag,
      d =>result_zero,
      q => flags(0) --zero
);

nFlag: DF
port map(
      clk => clk,
      rst =>rst,
      pre =>odin(4),
      ce  =>result_flag,
      d =>alu_out(7),
      q => flags(1) --negative
);

ac1 :AC
port map(mux2_out, ld_ac, reset, clk, ac_out);

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
port map(alu_out, dm_in_dbus, ac_src, mux2_out); 	


 opcode <= im_dbus(7 downto 5);
 im_abus <= pc_out;
 dm_abus <= im_dbus (4 downto 0);
 dm_out_dbus <= ac_out;

end Behavioral;

----------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use  IEEE.STD_LOGIC_ARITH.all;
use  IEEE.STD_LOGIC_UNSIGNED.all;
entity Controller is
port(
	opcode: 			in std_logic_vector(2 downto 0);
	flags:			in std_logic_vector(2 downto 0);
	rd_mem: 			out std_logic;
	result_flag: 	out std_logic;
	wr_mem: 			out std_logic;
	ac_src: 			out std_logic;
	alu_sel: 		out std_logic;
	ld_ac: 			out std_logic;
	pc_src: 			out std_logic
	);
end Controller;

architecture Behavioral of Controller is

begin
 process(opcode)
   begin
	case opcode is
  when "001" =>   				--lda adr
			rd_mem <= '1';
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '1';
			ac_src <= '1';
			alu_sel <= '0';
			pc_src <= '0';
	  when "010" =>					--add adr
  			rd_mem <= '1';
			result_flag <= '1';
			wr_mem <= '0';		
			ld_ac <= '1';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '0';
	when "011" =>					--dec adr
  			rd_mem <= '1';
			result_flag <= '1';
			wr_mem <= '0';		
			ld_ac <= '1';
			ac_src <= '0';
			alu_sel <= '1';
			pc_src <= '0';
  when "100" =>					-- sta adr
			rd_mem <= '0';
			result_flag <= '0';
			wr_mem <= '1';		
			ld_ac <= '0';  		
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '0';
  when "101" =>   					--jmp adr
			rd_mem <= '0';
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '1';
	  when "110" =>   					--jz adr
	  if (flags(0)='1') then			--flag zero
			rd_mem <= '0';
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '1';
		else
			rd_mem <= '0';  				--xxx
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '0';
		end if;
		
			  when "111" =>   			--jn adr
	  if (flags(1)='1') then			--flag negative
			rd_mem <= '0';
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '1';
		else
			rd_mem <= '0';  				--xxx
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
			alu_sel <= '0';
			pc_src <= '0';
		end if;

 when others =>					--xxx
			rd_mem <= '0';
			result_flag <= '0';
			wr_mem <= '0';		
			ld_ac <= '0';
			ac_src <= '0';
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
	 rst:			in std_logic;
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
	 clk: 			in std_logic;
	 rst:				in std_logic;
	 flags:			out std_logic_vector(2 downto 0);
	opcode:			out std_logic_vector(2 downto 0);
	im_abus:			out std_logic_vector(4 downto 0); --instruction addr bus
	im_dbus: 		in std_logic_vector(7 downto 0); --instruction data bus
	
	dm_abus:				out std_logic_vector(4 downto 0); --data mem adr bus
	dm_in_dbus: 		in std_logic_vector(7 downto 0);--data mem input data bus
	dm_out_dbus :		out std_logic_vector(7 downto 0)		--data mem output data bus
	 );
end component;

component Controller is
port(
	opcode: 			in std_logic_vector(2 downto 0);
	rd_mem: 			out std_logic;
	flags:			in std_logic_vector(2 downto 0);
	wr_mem: 			out std_logic;
	result_flag: 	out std_logic;
	ac_src: 			out std_logic;
	alu_sel:			out std_logic;
	ld_ac: 			out std_logic;
	pc_src: 			out std_logic
	);
end component;

signal opcode			: std_logic_vector(2 downto 0);
signal ac_src			: std_logic;
signal ld_ac			: std_logic;
signal pc_src			: std_logic;
signal alu_sel			: std_logic;
signal result_flag	: std_logic;
signal flags			: std_logic_vector(2 downto 0);

begin

dp1 :DataPath
port map(
	reset=>reset,
	ld_ac=>ld_ac,
	ac_src=>ac_src,
	result_flag=>result_flag,
	alu_sel => alu_sel,
	pc_src=>pc_src,
	clk=>clk,
	rst=>rst,
	flags=>flags,
	opcode=>opcode,
	im_abus=>im_abus,
	im_dbus=>im_dbus,
	
	dm_abus=>dm_abus,
	dm_in_dbus=>dm_in_dbus,
	dm_out_dbus=>dm_out_dbus 	
);



c1 :Controller
port map(
	opcode=>opcode,
	rd_mem=>rd_mem,
	flags=>flags,
	wr_mem=>wr_mem,
	result_flag=>result_flag,
	alu_sel => alu_sel,
	ac_src=>ac_src,
	ld_ac=>ld_ac,
	pc_src=>pc_src
);

end Behavioral;