;
; CS1022 Introduction to Computing II 2018/2019
; Lab 4 Part A
;
; Ted Johnson - 2020/04/03


TRUE		EQU	1
FALSE		EQU	0

; TIMER0 registers
T0TCR		EQU	0xE0004004
T0TC		EQU	0xE0004008

; Pin Control and GPIO registers
PINSEL4		EQU	0xE002C010
FIO2DIR1	EQU	0x3FFFC041
FIO2PIN1	EQU	0x3FFFC055


	AREA	RESET, CODE, READONLY
	ENTRY
	
	
	; === Setup ===
	
	; Load in common registers
	LDR	SP, =0x40010000		; initialize system stack pointer
	LDR	R6, =0x2FAF080		; int minTime = 0x2FAF080; // 50 seconds in microseconds (Takes ~5 seconds in simulation)
	LDR	R7, =0x4C4B400		; int maxTime = 0x4C4B400; // 80 seonds in microseconds  (Takes ~8 seconds in simulation)
	
	; Config P2.10 as a GPIO
	LDR	R4, =PINSEL4		;  v
	LDR	R5, [R4]		; read current PINSEL4 value
	BIC	R5, R5, #(0x3 << 20)	; clear bits 21:20 to make P2.10 GPIO functionality
	STR	R5, [R4]		; write the new value to PINSEL4
	
	; Config P2.10 as input
	LDR	R4, =FIO2DIR1		;  v
	LDRB	R5, [R4]		; read current FIO2DIR1 value
	BIC	R5, R5, #(0x1 << 2)	; clear bit 2 to make P2.10 input
	STRB	R5, [R4]		; write the new value to FIO2DIR1
	
game_roundStart
	
	; === Wait for the first button press ===
	
	LDR	R0, =FIO2PIN1		; param0 = FIO2PIN1;
	BL	waitBtnDn		; waitBtnDn(param0);
	
	
	; === Reset and Start the timer ===
	
	; Stop and reset TIMER0 using the Timer Control Register	
	LDR	R4, =T0TCR		;  v
	LDR	R5, =0x02		;  v
	STRB	R5, [R4]		; clear bit 0 and set bit 1 to stop and reset the timer
	
	; Set the Timer Counter to 0
	LDR	R4, =T0TC		;  v
	LDR	R5, =0			;  v
	STR	R5, [R4]		; set TC to zero
	
	; Start TIMER0 using the Timer Control Register
	LDR	R4, =T0TCR		;  v
	LDR	R5, =0x01		;  v
	STRB	R5, [R4]		; set bit 0 to start the timer
	
	
	; === Wait for the second button press ===
	
	LDR	R0, =FIO2PIN1		; param0 = PINGOESHERE;
	BL	waitBtnDn		; waitBtnDn(param0);
	
	
	; === Determine if the user won or lost ===
	
	LDR	R4, =T0TC		; 
	LDR	R8, [R4]		; int elapsedTime = T0TC;
	CMP	R8, R6			; if (elapsedTime < minTime)
	BLT	game_hasLost		;      goto game_hasLost;
	CMP	R8, R7			; if (elapsedTime > maxTime)
	BGT	game_hasLost		; 	goto game_hasLost;
	
	
	; === The user won, add 3 seconds to the target window and start a new round ===
	
	LDR	R8, =0x1C9C380		; timeIncrease = 0x1C9C380; // 30 seconds in microseconds (Takes ~3 seconds in simulation)
	ADD	R6, R6, R8		; minTime += timeIncrease;
	ADD	R7, R7, R8		; maxTime += timeIncrease;
	B	game_roundStart		; goto game_roundStart;
	
	
	; === The user has lost, turn on the LED and exit ===
	
game_hasLost
	; Config P2.10 as input
	LDR	R4, =FIO2DIR1		;  v
	LDRB	R5, [R4]		; read current FIO2DIR1 value
	ORR	R5, R5, #(0x1 << 2)	; set bit 2 to make P2.10 input
	STRB	R5, [R4]		; write the new value to PINSEL4
	
	; Turn on P2.10 LED
	LDR	R4, =FIO2PIN1		;  v
	LDRB	R5, [R4]		; read current FIO2PIN1 value
	BIC	R5, R5, #0x04		; clear bit 2 to turn on the LED
	STRB	R5, [R4]		; write the new value to FIO2PIN1
	
stop	B	stop


; waitBtnDn
; Waits until the button at address R0 is pressed
; before returning from the subroutine.
; parameters:
;	r0 - button pin address (memory location)
; result:
;	none
waitBtnDn
	PUSH	{R4-R6, LR}
	
	MOV	R4, R0		; pin = param0;
	LDRB	R5, [R4]	; curState = Memory.LoadByte(pin);
whBtnDn				; do {
	MOV	R6, R5		; 	lastState = curState;
	LDRB	R5, [R4]	;	curState = Memory.LoadByte(pin);
	TST	R5, #0x04	; } while (curState & 0x04 != 0x00 ||
	BNE	whBtnDn		;
	TST	R6, #0x04	;
	BEQ	whBtnDn		;
	
	POP	{R4-R6, PC}
	
	END