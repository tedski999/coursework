;
; CS1022 Introduction to Computing II 2018/2019
; Lab 4 Part B
;
; Ted Johnson - 2020/04/15
; Note: Plenty of boiler-plate code is taken straight
; from the CSU11022 button-int example

; TIMER0 registers
T0TCR		EQU	0xE0004004
T0TC		EQU	0xE0004008

; Pin Control, GPIO and ENT0 registers
FIO2DIR1	EQU	0x3FFFC041
FIO2PIN1	EQU	0x3FFFC055
PINSEL4		EQU	0xE002C010
EXTINT		EQU	0xE01FC140
EXTMODE		EQU	0xE01FC148
EXTPOLAR	EQU	0xE01FC14C

; Vector Interrupt Controller registers
VICIntSelect	EQU	0xFFFFF00C
VICIntEnable	EQU	0xFFFFF010
VICVectAddr0	EQU	0xFFFFF100
VICVectPri0	EQU	0xFFFFF200
VICVectAddr	EQU	0xFFFFFF00
	
; Program constants
VICVectEINT0	EQU	14
Irq_Stack_Size	EQU	0x80

; Modes
Mode_USR	EQU	0x10
Mode_IRQ	EQU	0x12
I_Bit		EQU	0x80
F_Bit		EQU	0x40

; Game contants assuming a 1Mhz clock input to TIMER0
START_MIN_TIME	EQU	5000000		; Round 1 has a minumum time of 5 seconds
TIME_WINDOW	EQU	3000000		; Round 1 has a maximum time of 5+3=8 seconds
TIME_INCREASE	EQU	3000000		; Round 2 will have a min time of 5+3=8 and max time of 8+3=11 seconds, etc...


	AREA	RESET, CODE, READONLY
	ENTRY
	
	B	Reset_Handler	; 0x00000000
	B	Undef_Handler	; 0x00000004
	B	SWI_Handler	; 0x00000008
	B	PAbt_Handler	; 0x0000000C
	B	DAbt_Handler	; 0x00000010
	NOP			; 0x00000014
	B	IRQ_Handler	; 0x00000018
	B	FIQ_Handler	; 0x0000001C
	

; Reset Exception Handler
Reset_Handler

	; Set Stack Pointer for each mode being used
	LDR	R0, =0x40010000				; 
	MSR     CPSR_c, #Mode_IRQ:OR:I_Bit:OR:F_Bit	; 
	MOV	SP, R0					; set IRQ Stack Pointer
	SUB	R0, R0, #0x80				; allocate 0x80 bytes for the IRQ stack
	MSR     CPSR_c, #Mode_USR			; change to USR mode
	MOV	SP, R0					; set USR Stack Pointer

	; === Setup P2.10 for EINT0 rising-edge ===

	; Enable P2.10 for EINT0 functionality
	LDR	R4, =PINSEL4		; 
	LDR	R5, [R4]		; read current PINSEL4 value
	BIC	R5, #(0x03 << 20)	; clear bits 21:20
	ORR	R5, #(0x01 << 20)	; set bits 21:20 to 01
	STR	R5, [R4]		; write the new value to PINSEL4

	; Set edge-sensitive mode for EINT0
	LDR	R4, =EXTMODE		; 
	LDR	R5, [R4]		; read current EXTMODE value
	ORR	R5, #1			; set bit 1
	STRB	R5, [R4]		; write the new value to EXTMODE

	; Set rising-edge mode for EINT0
	LDR	R4, =EXTPOLAR		; 
	LDR	R5, [R4]		; read current EXTPOLAR value
	BIC	R5, #1			; clear bit 1
	STRB	R5, [R4]		; write the new value to EXTPOLAR

	; Reset EINT0
	LDR	R4, =EXTINT		; 
	MOV	R5, #1			; 
	STRB	R5, [R4]		; write 1 to EXTINT

	
	; === Setup VIC for EINT0 ===

	; Common values for later code
	LDR	R4, =VICVectEINT0		; vector 14
	LDR	R5, =(1 << VICVectEINT0) 	; bit mask for vector 14

	; EINT0 should raise IRQs by clearing bit 4 of VICIntSelect
	LDR	R6, =VICIntSelect	; 
	LDR	R7, [R6]		; read current VICIntSelect value
	BIC	R7, R7, R5		; clear bit 14 (using the vector 14 bit mask)
	STR	R7, [R6]		; write the new value to VICIntSelect

	; Set priority for channel 14 to 15 (lowest)
	LDR	R6, =VICVectPri0	; 
	MOV	R7, #15			; 
	STR	R7, [R6, R4, LSL #2]	; write 15 to (VICVectPri0 + (VICVectEINT0 * 4))		-- this is index 4 of 4-byte values at VICVectPri0
	
	; Set the handler routine for channel 14 to Button_Handler
	LDR	R6, =VICVectAddr0	; 
	LDR	R7, =Button_Handler	; 
	STR	R7, [R6, R4, LSL #2]	; write Button_Handler to (VICVectAddr0 + (VICVectEINT0 * 4))	-- this is index 4 of 4-byte values at VICVectAddr0

	; Enable channel 14 by setting bit 14 of VICIntEnable
	LDR	R6, =VICIntEnable	; 
	STR	R5, [R6]		; set bit 14 of VICIntEnable (using the vector 14 bit mask)
	
	
	; === Setup TIMER0 ===
	
	BL	Reset_TIMER0		; Reset_TIMER0();
	
	
	; === Setup the game ===
	
	LDR	R4, =START_MIN_TIME	;
	LDR	R5, =MIN_TIME		; 
	STR	R4, [R5]		; Memory.storeWord(START_MIN_TIME, MIN_TIME);

STOP	B	STOP			; Idle until Exception

	
; Software Interrupt Exception Handler
Undef_Handler
	B	Undef_Handler

; Software Interrupt Exception Handler
SWI_Handler
	B	SWI_Handler

; Prefetch Abort Exception Handler
PAbt_Handler
	B	PAbt_Handler

; Data Abort Exception Handler
DAbt_Handler
	B	DAbt_Handler

; Interrupt ReQuest (IRQ) Exception Handler (top level - all devices)
IRQ_Handler
	SUB	LR, LR, #4		; adjust LR for IRQs by subrtacting 4
	STMFD	SP!, {R0-R3, LR}	; save R0-R3 and LR
	LDR	R0, =VICVectAddr	; get address of VIC Vector Address memory-mapped register
					;
	MOV	LR, PC			; manual BL due to branching to a different subroutine
	LDR	PC, [R0]		; dependant on the device raising the IRQ
					;
	LDMFD	SP!, {R0-R3, PC}^	; restore R0-R3, LR and CPSR
	
; Fast Interrupt reQuest Exception Handler
FIQ_Handler
	B	FIQ_Handler
	
; Button Press Exception Handler
Button_Handler
	STMFD	SP!, {R4-R7, LR}
	
	; Reset EINT0 interrupt by writing 1 to EXTINT register
	LDR	R4, =EXTINT
	MOV	R5, #1
	STRB	R5, [R4]
	
	; Do game logic (winning/losing the round)
	LDR	R4, =T0TCR
	LDRB	R4, [R4]		; int TIMER0_Started = T0TCR;
	AND	R4, R4, #0x1		; TIMER0_Started = TIMER0_Started & 0x01;
	CMP	R4, #0			; 
	BNE	BH_else			; if (TIMER0_Started == 0) {
	BL	Start_TIMER0		;	Start_TIMER0();	
	B	BH_eIf			; } else {
BH_else	LDR	R4, =T0TC		;	
	LDR	R4, [R4]		; 	int elapsedTime = T0TC;
	BL	Reset_TIMER0		; 	Reset_TIMER0();
	LDR	R7, =MIN_TIME		;
	LDR	R5, [R7]		;	int minTime = Memory.loadWord(MIN_TIME);
	LDR	R6, =TIME_WINDOW	;	int timeWindow = TIME_WINDOW;
	ADD	R6, R5, R6		;	int maxTime = minTime + timeWindow;
	CMP	R4, R5			; 	
	BLT	BH_lost			; 	
	CMP	R4, R6			;	
	BGT	BH_lost			; 	if (elapsedTime >= minTime || elapsedTime <= maxTime) {
	LDR	R6, =TIME_INCREASE	;		int timeIncrease = TIME_INCREASE;
	ADD	R5, R5, R6		;		minTime += timeIncrease;
	STR	R5, [R7]		;		Memory.storeWord(minTime, MIN_TIME);
	B	BH_eIf			;	} else {
BH_lost	BL	End_Game		;		EndGame();
					;	}
BH_eIf					; }

	; Clear source of interrupt
	LDR	R4, =VICVectAddr	; 
	MOV	R5, #0			; 
	STR	R5, [R4]		; Memory.storeWord(0, VICVectAddr);

	LDMFD	SP!, {R4-R7, PC}

; EndGame
; Disables EINT0 at P2.10 and enables P2.10s LED
; parameters:
;	none
; result:
;	none
End_Game
	PUSH	{R4-R5, LR}
	
	; Config P2.10 as a GPIO
	LDR	R4, =PINSEL4		; 
	LDR	R5, [R4]		; read current PINSEL4 value
	BIC	R5, R5, #(0x3 << 20)	; clear bits 21:20 to make P2.10 GPIO functionality
	STR	R5, [R4]		; write the new value to PINSEL4
	
	; Config P2.10 as output
	LDR	R4, =FIO2DIR1		; 
	LDRB	R5, [R4]		; read current FIO2DIR1 value
	ORR	R5, R5, #(0x1 << 2)	; set bit 2 to make P2.10 output
	STRB	R5, [R4]		; write the new value to PINSEL4
	
	; Turn on P2.10 LED
	LDR	R4, =FIO2PIN1		; 
	LDRB	R5, [R4]		; read current FIO2PIN1 value
	BIC	R5, R5, #0x04		; clear bit 2 to turn on the LED
	STRB	R5, [R4]		; write the new value to FIO2PIN1
	
	POP	{R4-R5, PC}

; Reset_TIMER0
; Sets T0TCR to 0x02 and T0TC to 0 to stops and reset TIMER0
; parameters:
;	none
; result:
;	none
Reset_TIMER0
	PUSH	{R4-R5, LR}

	; Stop and reset TIMER0 using T0TCR	
	LDR	R4, =T0TCR
	LDR	R5, =0x02
	STRB	R5, [R4]
	
	; Set the Timer Counter to 0
	LDR	R4, =T0TC
	LDR	R5, =0
	STR	R5, [R4]
	
	POP	{R4-R5, PC}

; Start_TIMER0
; Sets bit 0 of T0TCR to start TIMER0
; parameters:
;	none
; result:
;	none
Start_TIMER0
	PUSH	{R4-R5, LR}
	
	; Start TIMER0 using the Timer Control Register
	LDR	R4, =T0TCR
	LDR	R5, =0x01
	STRB	R5, [R4]
	
	POP	{R4-R5, PC}
	
	
; Preallocate RAM
	AREA	Data, DATA, READWRITE

MIN_TIME	SPACE	4


	END