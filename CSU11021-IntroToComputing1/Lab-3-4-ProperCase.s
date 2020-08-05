;
; CSU11021 Introduction to Computing I 2019/2020
; Proper Case
;
; Tested with following inputs:
;  "Hello World",0
;  "HELLO world",0
;  "HELLO   world",0
;  "",0
;  " ",0
;  "HELLO world!",0
;  "123",0
;
; Important Variables Table:
;	R0 - charAddr
;	R1 - outputAddr
;	R2 - isFirstChar
;	R3 - char
;

	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R0, =tststr	;	charAddr = GetAddr('tststr');
	LDR	R1, =0x40000000	;	outputAddr = 0x40000000;
	LDR	R2, =1		;	isFirstChar = true;
				;	
doWhile				;	do {
	LDRB	R3, [R0]	;		byte char = LoadByte(charAddr);
	CMP	R3, #' '	;		if (char == ' ')
	BNE	doElif		;		
	LDR	R2, =1		;			isFirstChar = true;
	B	eIfElse		;
doElif	CMP	R2, #1		;		else if (isFirstChar == true)
	BNE	doElse		;
	LDR	R2, =0		;			isFirstChar = false;
	CMP	R3, #'a'	;			if (char >= 'a'
	BLO	eIfElse		;				&&
	CMP	R3, #'z'	;			    char <= 'z')
	BHI	eIfElse		;
	SUB	R3, R3, #0x20	;				char -= 0x20;
	B	eIfElse		;
doElse				;		else
	LDR	R2, =0		;			isFirstChar = false;
	CMP	R3, #'A'	;			if (char >= 'A'
	BLO	eIfElse		;				&&
	CMP	R3, #'Z'	;			    char <= 'Z')
	BHI	eIfElse		;
	ADD	R3, R3, #0x20	;				char += 0x20;
eIfElse				;
	STRB	R3, [R1]	;		StoreByte(outputAddr, char);
	ADD	R0, R0, #1	;		charAddr++;
	ADD	R1, R1, #1	;		outputAddr++;
	CMP	R3, #0x00	;	} while (char != 0);
	BNE	doWhile		;

STOP	B	STOP

tststr	DCB	"hELLO   world",0

	END
