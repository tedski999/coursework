
`timescale 1ns/100ps
`default_nettype none

module OddFunction (A, x, y, clk, reset_b);
	output		A;
	input		x, y, clk, reset_b;

	reg			A;
	parameter	S0 = 1'b0, S1 = 1'b1;

	always @(posedge clk, negedge reset_b)
	begin
		if (reset_b == 1'b1)
		begin
			case (A)
				S0: if (x == y) A <= S0; else A <= S1;
				S1: if (x == y) A <= S1; else A <= S0;
			endcase
		end
		else
			A <= S0;
	end

endmodule // OddFunction