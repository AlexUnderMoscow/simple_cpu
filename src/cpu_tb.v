`timescale 1ns / 1ps


module InstMemory (
	input [4:0] abus,
	output reg [7:0] dbus
);
reg [7:0] im_array [0:31];
always @(abus) dbus = im_array[abus];
initial begin
//program add dec sta jz
	/*im_array[0] = 8'h22; //lda 02
	im_array[1] = 8'h41; //add 01
	im_array[2] = 8'h83; //sta 03
	im_array[3] = 8'hC0; //jz 00
	
	im_array[4] = 8'h21; //lda 01
	im_array[5] = 8'h60; //dec 00
	im_array[6] = 8'h81; //sta 01
	im_array[7] = 8'h22; //lda 02
	im_array[8] = 8'h61; //dec 01
	im_array[9] = 8'h83; //sta 03
	im_array[10] = 8'hE4; //jn 04
	im_array[11] = 8'hA0; //jmp 00*/
	
	/*program stack*/
	/*im_array[0] 	= 8'h20; //lda 00
	im_array[1] 	= 8'h01; //push
	im_array[2] 	= 8'h21; //lda 01
	im_array[3] 	= 8'h01; //push
	im_array[4] 	= 8'h22; //lda 02
	im_array[5] 	= 8'h01; //push
	im_array[6] 	= 8'h00; //nop
	im_array[7] 	= 8'h02; //pop
	im_array[8] 	= 8'h83; //sta 03
	im_array[9] 	= 8'h02; //pop
	im_array[10] 	= 8'h84; //sta 04
	im_array[11] 	= 8'h02; //pop
	im_array[12] 	= 8'h85; //sta 05
	im_array[13] 	= 8'h1F; //halt*/
	
		/*program mov stack*/
	/*im_array[0] 	= 8'h03; //mov
	im_array[1] 	= 8'h77; // 77
	im_array[2] 	= 8'h01; //push
	
	im_array[3] 	= 8'h03; //mov
	im_array[4] 	= 8'h88; // 88
	im_array[5] 	= 8'h01; //push
	
	im_array[6] 	= 8'h03; //mov
	im_array[7] 	= 8'h99; // 99
	im_array[8] 	= 8'h01; //push

	im_array[9] 	= 8'h00; //nop
	im_array[10] 	= 8'h02; //pop
	im_array[11] 	= 8'h83; //sta 03
	im_array[12] 	= 8'h02; //pop
	im_array[13] 	= 8'h84; //sta 04
	im_array[14] 	= 8'h02; //pop
	im_array[15] 	= 8'h85; //sta 05
	im_array[16] 	= 8'h1F; //halt*/
	
	//program y=ax+b
	im_array[0] 	= 	8'h03; 	//mov
	im_array[1] 	= 	8'h07; 	//7		a
	im_array[2] 	= 	8'h80; 	//sta 00
	im_array[3] 	= 	8'h03; 	//mov
	im_array[4] 	= 	8'h04; 	//4		x
	im_array[5] 	= 	8'h81; 	//sta 01
	im_array[6] 	= 	8'h03; 	//mov
	im_array[7] 	= 	8'h06; 	//6  	b
	im_array[8] 	= 	8'h82; 	//sta 02
	im_array[9] 	= 	8'h03; 	//mov
	im_array[10] 	= 	8'h00; 	//0  	res
	im_array[11] 	= 	8'h83; 	//sta 03
	im_array[12] 	= 	8'h03; 	//mov
	im_array[13] 	= 	8'h01; 	//1  	"1"
	im_array[14] 	= 	8'h84; 	//sta 04
	im_array[15] 	= 	8'h23; 	//lda 03 res
	im_array[16] 	= 	8'h40; 	//add 00 a
	im_array[17] 	= 	8'h83; 	//sta 03 res
	im_array[18] 	= 	8'h21; 	//lda 01 x
	im_array[19] 	= 	8'h64; 	//dec 04 "1"
	im_array[20] 	= 	8'h81; 	//sta 01 x
	im_array[21] 	= 	8'hD7; 	//jz 0x17 d23
	im_array[22] 	= 	8'hAF; 	//jmp 0F
	im_array[23] 	= 	8'h23; 	//lda 03 res
	im_array[24] 	= 	8'h42; 	//add 02 b
	im_array[25] 	= 	8'h83; 	//sta 03 res
	im_array[26] 	= 	8'h1F; 	//halt*/
	

end
endmodule

module DataMemory (
	input clk,
	input rd, wr, 
	input [4:0] abus,
	input [7:0] in_dbus, 
	output reg [7:0] out_dbus
);
reg [7:0] dm_array [0:31];
always @(*)
begin
 if (rd) out_dbus = dm_array[abus];
end

always @(negedge clk) // or abus
begin
if (wr) dm_array[abus] = in_dbus;
end
initial begin 
  dm_array[0] = 8'h01;
  dm_array[1] = 8'h07;
  dm_array[2] = 8'h05;
  dm_array[3] = 8'h00;
  dm_array[4] = 8'h00;
  dm_array[5] = 8'h00;
  dm_array[6] = 8'h00;
end
endmodule
/**/

module cpu_tb;

	reg clk;
	reg reset;
	wire [4:0] im_abus, dm_abus;
	wire [7:0] im_dbus, dm_in_dbus, dm_out_dbus;
	wire rd_mem, wr_mem;

	// Instantiate the Unit Under Test (UUT)
	smpl_vhdl uut (
		.clk(clk), 
		.reset(reset), 
		.rd_mem(rd_mem), 
		.wr_mem(wr_mem), 
		.im_abus(im_abus), 
		.im_dbus(im_dbus), 
		.dm_abus(dm_abus), 
		.dm_in_dbus(dm_in_dbus), 
		.dm_out_dbus(dm_out_dbus)
	);
	
	InstMemory IM(im_abus,im_dbus);
	DataMemory DM(clk, rd_mem, wr_mem, dm_abus, dm_out_dbus, dm_in_dbus);

	initial begin
		// Initialize Inputs
		clk = 1'b0;
		reset = 1'b0;
		#5 reset = 1'b1;
		#20 reset = 1'b0;
	//	#500 $finish;
	end
   
	always #10 clk = ~clk;
      
endmodule

