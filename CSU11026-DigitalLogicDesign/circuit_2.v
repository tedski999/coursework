
`timescale 1ns/100ps
`default_nettype none

module Circuit_2 (Out_1, Out_2, Out_3, A, B, C, D);

	input A, B, C, D;
	output Out_1, Out_2, Out_3;

	// NOT inputs
	wire nA, nB, nC, nD;
	not (nA, A);
	not (nB, B);
	not (nC, C);
	not (nD, D);

	// Out_1 = (A + !B)(!C)(C + D)
	wire T1, T2;
	or (T1, A, nB);
	or (T2, C, D);
	and (Out_1, T1, nC, T2);

	// Out_2 = (!CD + BCD + C!D)(!A + B)
	wire T3, T4, T5, T6, T7;
	and (T3, nC, D);
	and (T4, B, C, D);
	and (T5, C, nD);
	or (T6, T3, T4, T5);
	or (T7, nA, B);
	and (Out_2, T6, T7);

	// Out_3 = (AB + C)D + !BC
	wire T8, T9, T10, T11;
	and (T8, A, B);
	or (T9, T8, C);
	and (T10, T9, D);
	and(T11, nB, C);
	or (Out_3, T10, T11);

endmodule // Circuit_2


/*
	Expected outputs:
	0000 = 0 0 0
	0001 = 1 1 0
	0010 = 0 1 1
	0011 = 0 0 1
	0100 = 0 0 0
	0101 = 0 1 0
	0110 = 0 1 0
	0111 = 0 1 1
	1000 = 0 0 0
	1001 = 1 0 0
	1010 = 0 0 1
	1011 = 0 0 1
	1100 = 0 0 0
	1101 = 1 1 1
	1110 = 0 1 0
	1111 = 0 1 1
*/
