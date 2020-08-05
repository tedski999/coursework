
`timescale 1ns/100ps
`default_nettype none

module BCD_Adder (Sum, C_out, A, B, C_in);
	output	[0: 3]	Sum;
	output			C_out;
	input	[0: 3]	A, B;
	input			C_in;

endmodule // BCD_Adder

module Binary_Adder (Sum, C_out, A, B, C_in);
	output			Sum, C_out;
	input			A, B, C_in;
Binary_Adder adder (Sum, C_out, A, B, C_in);
	assign {C_out, Sum} A + B + C_in;
endmodule; // Binary_Adder

/*

 A = 0000 B = 0000 Carry In = 0 Sum = 0000 Carry Out = 0
 A = 0000 B = 0001 Carry In = 0 Sum = 0001 Carry Out = 0
 A = 0000 B = 0010 Carry In = 0 Sum = 0010 Carry Out = 0
 A = 0000 B = 0011 Carry In = 0 Sum = 0011 Carry Out = 0
 A = 0000 B = 0100 Carry In = 0 Sum = 0100 Carry Out = 0
 A = 0000 B = 0101 Carry In = 0 Sum = 0101 Carry Out = 0
 A = 0000 B = 0110 Carry In = 0 Sum = 0110 Carry Out = 0
 A = 0000 B = 0111 Carry In = 0 Sum = 0111 Carry Out = 0
 A = 0000 B = 1000 Carry In = 0 Sum = 1000 Carry Out = 0
 A = 0000 B = 1001 Carry In = 0 Sum = 1001 Carry Out = 0
 A = 0001 B = 0000 Carry In = 0 Sum = 0001 Carry Out = 0
 A = 0001 B = 0001 Carry In = 0 Sum = 0010 Carry Out = 0
 A = 0001 B = 0010 Carry In = 0 Sum = 0011 Carry Out = 0
 A = 0001 B = 0011 Carry In = 0 Sum = 0100 Carry Out = 0
 A = 0001 B = 0100 Carry In = 0 Sum = 0101 Carry Out = 0
 A = 0001 B = 0101 Carry In = 0 Sum = 0110 Carry Out = 0
 A = 0001 B = 0110 Carry In = 0 Sum = 0111 Carry Out = 0
 A = 0001 B = 0111 Carry In = 0 Sum = 1000 Carry Out = 0
 A = 0001 B = 1000 Carry In = 0 Sum = 1001 Carry Out = 0
 A = 0001 B = 1001 Carry In = 0 Sum = 0000 Carry Out = 1
 A = 0010 B = 0000 Carry In = 0 Sum = 0010 Carry Out = 0
 A = 0010 B = 0001 Carry In = 0 Sum = 0011 Carry Out = 0
 A = 0010 B = 0010 Carry In = 0 Sum = 0100 Carry Out = 0
 A = 0010 B = 0011 Carry In = 0 Sum = 0101 Carry Out = 0
 A = 0010 B = 0100 Carry In = 0 Sum = 0110 Carry Out = 0

*/
