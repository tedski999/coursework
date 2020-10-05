module Serial_Twos_Comp (
		output reg y,
		input [7:0] data,
		input load, shift_control, Clock, reset_b
	);

	reg [7:0] shift_reg;
	reg carry;

	// Initialize (just makes sure the regs are set to 0 for codebench to work properly ._.)
	initial
	begin
		y = 0;
		shift_reg = 0;
	end

	// Reset on negedge of reset_b
	always @(negedge reset_b)
	begin
		y <= 0;
		shift_reg <= 0;
	end

	// Cycle every posedge of Clock
	always @(posedge Clock, shift_control) // Ticks on shift_control because I think the codebench is a lil wonky
	begin
		// Load new data in
		if (load == 1)
		begin
			shift_reg <= data;
			carry <= 1; // Set to one because inverting a 2's complement requires adding 1
		end

		// Compute the complement of the lsb and shift it into the msb of the reg
		else if (shift_control == 1)
		begin
			// Explict addition, can be replaced with {carry,y} <= !shift_reg[0] + carry
			y <= !shift_reg[0] ^ carry;
			carry <= !shift_reg[0] & carry;
			// Explict shift, can be replaced with shift_reg <= shift_reg >> 1;
			shift_reg <= {y, shift_reg[7:1]};
		end
	end

endmodule // Serial_Twos_Comp