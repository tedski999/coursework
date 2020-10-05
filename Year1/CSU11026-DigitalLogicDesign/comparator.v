
`timescale 1ns/100ps
`default_nettype none

module Compare (A, B, Y);

	input [0:3]		A, B;
	output [0:5]	Y;
	reg	[0:5]		Y;

	always @(A or B)
	begin
		Y = 0;
		if (A == B)	Y[0] = 1;
		else		Y[1] = 1;
		if (A > B)	Y[2] = 1;
		else		Y[5] = 1;
		if (A >= B)	Y[4] = 1;
		else		Y[3] = 1;
	end

endmodule // Compare