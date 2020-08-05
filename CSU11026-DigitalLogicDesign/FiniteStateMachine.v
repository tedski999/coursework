
`timescale 1ns/100ps
`default_nettype none

module FiniteStateMachine (out_z, in_x, in_y, clk, reset_b);
	output		out_z;
	input		in_x, in_y, clk, reset_b;

	reg[0:1]	state;
	parameter	S0 = 2'b00, S1 = 2'b01, S2 = 2'b10, S3 = 2'b11;

	// Reset state on negedge of reset_b
	always @(negedge reset_b)
	begin
		state <= S0;
	end

	always @(posedge clk)
	begin

		// Update current state
		// Reset if in_x is low
		if (in_x == 1'b0)
			state <= S0;

		// Go to next state if in_x is high
		else
		begin
			case (state)
				S0: if (in_y == 1'b0) state <= S3; else state <= S1;
				S1: state <= S2;
				S2: state <= S3;
				S3: state <= S3;
			endcase
		end
	end

	// Update out_z wire
	assign out_z = state[0];

endmodule // FiniteStateMachine