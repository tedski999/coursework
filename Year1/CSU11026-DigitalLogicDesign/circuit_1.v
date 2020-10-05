
`timescale 1ns/100ps
`default_nettype none

module Circuit_1 (A, B, C, F1, F2);

	input A, B, C;
	output F1, F2;

	// Segment 1 Logic
	wire T1, T2;
	and	G1 (T1, A, B, C);
	or	G2 (T2, A, B, C);

	// Segment 2 Logic
	wire T3, T4, T5;
	and	G3 (T3, A, B);
	and	G4 (T4, A, C);
	and	G5 (T5, B, C);
	or	G6 (F2, T3, T4, T5);

	// Segment 3 Logic
	wire T6, T7;
	not	G7 (T6, F2);
	and	G8 (T7, T2, T6);
	or	G9 (F1, T1, T7);

endmodule // Circuit_1
