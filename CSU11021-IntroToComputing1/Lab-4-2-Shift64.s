;
; CSU11021 Introduction to Computing I 2019/2020
; 64-bit Shift
;

	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R1, =0xD9448A9B		; most significaint 32 bits (63 ... 32)
	LDR	R0, =0xB8AA9D3B		; least significant 32 bits (31 ... 0)
	LDR	R2, =4			; shift count
	
ifPos	CMP	R2, #0			;	if while (shiftCount > 0)
	BLE	ifNeg			;	{
	MOV	R0, R0, LSR #1		;		leastSigBits >> 1;
	MOVS	R1, R1, LSR #1		;		mostSigBits >> 1;
	BCC	sCarryP			;		if (Stats.carryOut == 1)
	ORR	R0, R0, #0x80000000	;			leastSigBits = leastSigBits | 0x80000000;
sCarryP	SUB	R2, R2, #1		;		shiftCount -= 1;
	B	ifPos			;	}
ifNeg	CMP	R2, #0			;	else while (shiftCount < 0)
	BGE	eIf			;	{
	MOV	R1, R1, LSL #1		;		mostSigBits << 1;
	MOVS	R0, R0, LSL #1		;		leastSigBits << 1;
	BCC	sCarryN			;		if (Stats.carryOut == 1)
	ORR	R1, R1, #0x00000001	;			mostSigBits = mostSigBits | 0x00000001;
sCarryN	ADD	R2, R2, #1		;		shiftsRemaining += 1;
	B	ifNeg			;	}
eIf

STOP	B	STOP

	END
