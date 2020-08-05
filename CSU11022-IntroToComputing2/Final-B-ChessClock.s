;
; CS1022 Introduction to Computing II 2018/2019
; Chess Clock
;
; Ted Johnson - 2020/08/05
; Note:	Plenty of boiler-plate code is taken straight
; 	from the CSU11022 timer-int and button-int examples,
; 	primarily for initialization

; TIMER0
T0IR		EQU	0xE0004000
T0TCR		EQU	0xE0004004
T0TC		EQU	0xE0004008
T0MR0		EQU	0xE0004018
T0MCR		EQU	0xE0004014

; Pin Control and ENT0
PINSEL4		EQU	0xE002C010
FIO2DIR1	EQU	0x3FFFC041
FIO2PIN1	EQU	0x3FFFC055
EXTINT		EQU	0xE01FC140
EXTMODE		EQU	0xE01FC148
EXTPOLAR	EQU	0xE01FC14C

; Vector Interrupt Controller
VICIntSelect	EQU	0xFFFFF00C
VICIntEnable	EQU	0xFFFFF010
VICVectAddr0	EQU	0xFFFFF100
VICVectPri0	EQU	0xFFFFF200
VICVectAddr	EQU	0xFFFFFF00
VICVectT0	EQU	4
VICVectEINT0	EQU	14

; Modes
Mode_USR        EQU     0x10
Mode_IRQ        EQU     0x12
I_Bit           EQU     0x80
F_Bit           EQU     0x40
Irq_Stack_Size	EQU	0x80

; Program constants - assuming a 1MHz clock, this allows
; a total of 10 minutes (600 seconds) for each player
COUNTDOWN_LENGTH	EQU	600000000
PLAYER1			EQU	1
PLAYER2			EQU	2


	AREA	RESET, CODE, READONLY
	ENTRY

	; Exception Vectors

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
	LDR	R5, =0x01		; 
	STRB	R5, [R4]		; write 00000001 to EXTINT
	
	
	; === Setup TIMER0 for Match Interrupt ===
	
	; Stop and reset TIMER0 using the TCR
	; - bit 0 to 0 to stop
	; - bit 1 to 1 to reset
	LDR	R4, =T0TCR		; 
	LDR	R5, =0x02		; 
	STRB	R5, [R4]		; write 00000010 to T0TCR
	
	; Clear any previous TIMER0 interrupt by overwriting T0IR with ones
	LDR	R4, =T0IR		; 
	LDR	R5, =0xFF		; 
	STRB	R5, [R4]		; write 11111111 to T0IR
	
	; Raise an IRQ on TIMER0 match using the MCR
	; - bit 0 to 1 to turn on interrupts
	; - bit 1 to 0 to maintain counter after every match
	; - bit 2 to 1 to stop the counter after match
	LDR	R4, =T0MCR		; 
	LDR	R5, =0x05		; 
	STRH	R5, [R4]		; write 00000101 to T0MCR
	
	
	; === Setup VIC for EINT0 and TIMER0 match ===
	
	; Common values for later code
	LDR	R4, =VICVectT0		; vector 4 for TIMER0 match
	LDR	R5, =VICVectEINT0	; vector 14 for EINT0
	LDR	R6, =(1 << VICVectT0)	; create a bit mask of vector 4
	LDR	R7, =(1 << VICVectEINT0); create a bit mask of vector 14
	ORR	R6, R6, R7		; combine both the bit masks
	
	; TIMER0 and EINT0 will raise IRQs by clearing bit 4 and 14 of VICIntSelect
	LDR	R7, =VICIntSelect	; 
	LDR	R8, [R7]		; read current VICIntSelect value
	BIC	R8, R8, R6		; clear both the bits using the combined bit mask
	STR	R8, [R7]		; write the new value to VICIntSelect
	
	; Set priority for channel 4 to 14 and for channel 14 to 15
	LDR	R7, =VICVectPri0	; 
	MOV	R8, #14			; 
	STR	R8, [R7, R4, LSL #2]	; write 14 to (VICVectPri0 + (VICVectT0 * 4))
	MOV	R8, #15			; 
	STR	R8, [R7, R5, LSL #2]	; write 15 to (VICVectPri0 + (VICVectEINT0 * 4))
	
	; Set the handler routines for channel 4 to Timer_Handler and for channel 14 to Button_Handler
	LDR	R7, =VICVectAddr0	; 
	LDR	R8, =Timer_Handler	; 
	STR	R8, [R7, R4, LSL #2]	; write Button_Handler to (VICVectAddr0 + (VICVectEINT0 * 4))
	LDR	R8, =Button_Handler	; 
	STR	R8, [R7, R5, LSL #2]	; write Button_Handler to (VICVectAddr0 + (VICVectEINT0 * 4))
	
	
	; === Setup the program ===
	
	; Save player 1s initial remaining time to memory
	LDR	R4, =P1_TIME		; 
	LDR	R5, =COUNTDOWN_LENGTH	; 
	STR	R5, [R4]		; store COUNTDOWN_LENGTH to P1_TIME
	
	; Save player 2s initial remaining time to memory
	LDR	R4, =P2_TIME		; 
	LDR	R5, =COUNTDOWN_LENGTH	; 
	STR	R5, [R4]		; store COUNTDOWN_LENGTH to P2_TIME
	
	; Save the current turn as player 2s initially, therefore it will be
	; player 1s turn when the button is first pressed to start the clock
	LDR	R4, =CUR_TURN		; 
	LDR	R5, =PLAYER2		; 
	STRB	R5, [R4]		; store PLAYER2 to CUR_TURN

	
	; === Enable interrupts ===
	
	; Enable channel 4 and 14 by setting bit 4 and 14 of VICIntEnable
	LDR	R7, =VICIntEnable	; 
	STR	R6, [R7]		; set bit 4 and 14 of VICIntEnable using the combined bit mask

STOP	B	STOP			; idle until exception raised


;
; TOP LEVEL EXCEPTION HANDLERS
;

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
	
	MOV	LR, PC			; manual BL due to branching to a different subroutine
	LDR	PC, [R0]		; dependant on the device raising the IRQ
	
	LDMFD	SP!, {R0-R3, PC}^	; restore R0-R3, LR and CPSR

; Fast Interrupt reQuest Exception Handler
FIQ_Handler
	B	FIQ_Handler


; Button Press Exception Handler
Button_Handler
	PUSH	{R4-R7, LR}
	
	; Reset EINT0 interrupt by writing 0x01 to EXTINT register
	LDR	R4, =EXTINT		;
	LDR	R5, =0x01		;
	STRB	R5, [R4]		; write 00000001 to EXTINT to reset EINT0 interrupt
	
	; Get the elapsed time since the last time the button was pressed and reset TIMER0
	LDR	R5, =T0TC		; 
	LDR	R4, [R5]		; get the elapsedTime from T0TC
	LDR	R5, =T0TCR		; 
	LDR	R6, =0x03		; 
	STR	R6, [R5]		; write 00000011 to T0TC to reset TIMER0 counter
	
	; Determine which countdown to update and which to set TIMER0 to
	LDR	R5, =CUR_TURN		;	
	LDR	R6, [R5]		;	curTurn = Memory.load(CUR_TURN);
	CMP	R6, #PLAYER1		;	if (curTurn == PLAYER1)
	BNE	Button_Handler_else	;	{
	LDR	R6, =PLAYER2		;		
	STRB	R6, [R5]		;		Memory.storeByte(CUR_TURN, PLAYER2);
	LDR	R5, =P1_TIME		;		lastPlayerTimeAddr = P1_TIME;
	LDR	R6, =P2_TIME		;		nextPlayerTimeAddr = P2_TIME;
	B	Button_Handler_endIf	;	}
Button_Handler_else			;	else {
	LDR	R6, =PLAYER1		;		
	STRB	R6, [R5]		;		Memory.storeByte(CUR_TURN, PLAYER1);
	LDR	R5, =P2_TIME		;		lastPlayerTimeAddr = P2_TIME;
	LDR	R6, =P1_TIME		;		nextPlayerTimeAddr = P1_TIME;
Button_Handler_endIf			;	}

	; Update the remaining time of the last player in memory
	LDR	R7, [R5]		; load lastPlayerTimeRemaining from lastPlayerTimeAddr
	SUB	R7, R7, R4		; subtract the elapsedTime from lastPlayerTimeRemaining
	STR	R7, [R5]		; store lastPlayerTimeRemaining at lastPlayerTimeAddr

	; Set TIMER0 match register to the new countdown
	LDR	R4, =T0MR0		; 
	LDR	R5, [R6]		; load nextPlayerTimeRemaining from nextPlayerTimeAddr
	STR	R5, [R4]		; write nextPlayerTimeRemaining to T0MR0 to set the match regester of TIMER0

	; Reset TIMER0 interrupt in case one was raised during initialization or changing the match
	LDR	R4, =T0IR		;
	LDR	R5, =0xFF		;
	STRB	R5, [R4]		; write 0xFF to T0IR

	; Start TIMER0 again with the new countdown
	LDR	R4, =T0TCR		; 
	LDR	R5, =0x01		; 
	STRB	R5, [R4]		; write 0x01 to T0TCR to start TIMER0

	; Clear source of interrupt
	LDR	R4, =VICVectAddr	; 
	MOV	R5, #0			; 
	STR	R5, [R4]		; write 0 to VICVectAddr to clear interrupt

	POP	{R4-R7, PC}


; Timer Exception Handler
Timer_Handler
	PUSH	{R4-R5, LR}
	
	; Reset TIMER0 interrupt by writing 0xFF to T0IR
	LDR	R4, =T0IR		;
	LDR	R5, =0xFF		;
	STRB	R5, [R4]		; write 0xFF to T0IR
	
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

	; Clear source of interrupt by writing 0 to VICVectAddr
	LDR	R4, =VICVectAddr	; 
	MOV	R5, #0			; 
	STR	R5, [R4]		; write 0 to VICVectAddr

	POP	{R4-R5, PC}


; Allocate 9 bytes of space in RAM for two player countdown timers (8) and whoes turn it is (1)
	AREA	globals, DATA, READWRITE

P1_TIME		SPACE	4			; 4 bytes for player 1s remaining time
P2_TIME		SPACE	4			; 4 bytes for player 2s remaining time
CUR_TURN	SPACE	1			; 1 byte for which players turn it is

	END
