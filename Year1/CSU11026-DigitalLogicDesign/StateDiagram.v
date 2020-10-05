`timescale 1ns/100ps
`default_nettype none

module StateDiagram (
		output reg y_out,
		input x_in, clk, reset_b
	);
	reg[0:2]	state;
	parameter	S0 = 3'b000, S1 = 3'b001, S2 = 3'b010, S3 = 3'b011, S4 = 3'b100;

	// Initialize the state
	initial
	begin
		state = S0;
		y_out = 1'b0;
	end

	// Reset state on negedge of reset_b
	always @(negedge reset_b)
	begin
		state <= S0;
	end

	// Cycle every posedge of clk
	always @(posedge clk)
	begin
		case (state)

			S0:
			begin
				if (x_in == 1'b1)
				begin
					state <= S1;
					y_out <= 1'b1;
				end
				else
					y_out <= 1'b0;
			end

			S1:
			begin
				state <= S2;
				y_out <= 1'b1;
			end

			S2:
			begin
				state <= S3;
				y_out <= 1'b1;
			end

			S3:
			begin
				state <= S4;
				y_out <= 1'b0;
			end

			S4:
			begin
				state <= S0;
				y_out <= 1'b0;
			end

			default: y_out <= 1'b0;
		endcase
	end

endmodule // StateDiagram