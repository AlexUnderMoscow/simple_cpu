`timescale 1ns / 1ps


module InstMemory (
	input [4:0] abus,
	output reg [7:0] dbus
);
reg [7:0] im_array [0:31];
always @(abus) dbus = im_array[abus];
initial begin
	im_array[0] = 8'h22; //lda 02
	im_array[1] = 8'h41; //add 01
	im_array[2] = 8'h83; //sta 03
	im_array[3] = 8'hC0; //jz 00
	
	im_array[4] = 8'h21; //lda 01
	im_array[5] = 8'h60; //dec 01
	im_array[6] = 8'h81; //sta 03
	im_array[7] = 8'h22; //lda 02
	im_array[8] = 8'h61; //dec 01
	im_array[9] = 8'h83; //sta 03
	im_array[10] = 8'hE4; //jn 04
	//im_array[11] = 8'h01F; //halt
	im_array[11] = 8'hA0; //jmp 00
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

always @(posedge clk) // or abus
begin
if (wr) dm_array[abus] = in_dbus;
end
initial begin 
  dm_array[0] = 8'h01;
  dm_array[1] = 8'h07;
  dm_array[2] = 8'h05;
  dm_array[3] = 8'h00;
end
endmodule
/**/

module cpu_tb;

	reg clk, rst;
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
		rst = 1'b0;
		reset = 1'b0;
		#5 reset = 1'b1;
		#20 reset = 1'b0;
	//	#500 $finish;
	end
   
	always #10 clk = ~clk;
      
endmodule

