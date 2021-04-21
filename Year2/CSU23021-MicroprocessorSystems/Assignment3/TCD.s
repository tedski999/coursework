; This program outputs the time since initialisation in HH:MM:SS format to the
; GPIO1 pins. Each character is represented by 4 pins, where a digit is a BCD
; value and the ':' character is all ones. For example, '12:34:56' would appear
; as '0001 0010 1111 0011 0100 1111 0101 0110' on the GPIO1 pin output.
; TIMER0 is set up to raise an IRQ exception every second. The IRQ handler
; increments a value stored in memory by 1. In effect, this value increases by
; 1 every second.
;
; The main program continuously reads this value.  If the value is greater or
; equal to 60, the value is reset to 0. As such, this value is interpretted as
; the number of seconds passed within the minute. After every reset, a counter
; which represents the number of minutes passed within an hour is incremented.
; This same pattern is applied between this minute counter and an hour counter.
; Using these three counters, we have three values that represent the hours,
; minutes and seconds which have passed since initialisation.
;
; Finially, these values are converted to 4 bit wide BCD. With the assumption
; that values can never be greater than 99, we can simply divide by 10 to find
; the 10s unit and use the remainder as the 1s unit. The remaining code simply
; separates each pair of BCD characters with '1111'.
;
; Known limitations:
; - If the program was blocked from executing for more than a second, it is
;   possible (even if extremely unlikely) for data corruption to occur as the
;   handler may attempt to increment the seconds counter in memory while the
;   main program was in the middle of resetting the counter. As such, the
;   value would be incremented but then immediately overwritten with a zero,
;   thus missing a full second.
; - The number of hours is reset to 0 after the value reaches more than 99.
;   This does not effect the output as the number of hours is limited to 2
;   digits anyway.
;
; Source code indented assuming 8-spaced tabs.
; Ted Johnson, 2021

	area tcd,code,readonly
	export __main

__main

; The number of 'ticks' per second
CLK2SEC	equ 14745600

; GPIO1
GPIO1	equ 0xE0028010
PINS	equ 0x0
DIRS	equ 0x8

; TIMER0
TIMER0	equ 0xE0004000
IR	equ 0x00
TCR	equ 0x04
MCR	equ 0x14
MR0	equ 0x18

; Vector Interrupt Controller
VIC		equ 0xFFFFF000
IntEnable	equ 0x010
VectAddr	equ 0x030
VectAddr0	equ 0x100
VectCtrl0	equ 0x200

; Modes we're interested in
Mode_USR	equ 0x10

	; === Drop to User mode if we haven't already ===
	
	msr CPSR_c, #Mode_USR

	; === Setup TIMER0 for Match Interrupt ===

	ldr r0,=TIMER0

	; Stop and reset TIMER0
	mov r1,#0x2
	str r1,[r0,#TCR]

	; Clear any previous TIMER0 interrupt by overwriting T0IR with 1s
	mov r1,#0xFF
	str r1,[r0,#IR]

	; Set TIMER0 MR0 to 1 second
	ldr r1,=(1 * CLK2SEC)
	str r1,[r0,#MR0]

	; Raise an IRQ on TIMER0 match, resetting counter after a match
	mov r1,#0x3
	str r1,[r0,#MCR]

	; Start TIMER0
	mov r1,#0x1
	str r1,[r0,#TCR]

	; === Setup GPIO1 pins ===

	; Set all 32 pins as outputs
	ldr r0,=(GPIO1 + DIRS)
	mov r1,#0xFFFFFFFF
	str r1,[r0]

	; === Setup VIC for TIMTER0 Match Interrupt ===

	ldr r0,=VIC

	; Set Vectored Interrupt 0 handler address to IRQHandler
	ldr r1,=IRQHandler
	str r1,[r0,#VectAddr0]

	; Set Vectored Interrupt 0 source to channel 4 (TIMER0)
	mov r1,#4 + (1 << 5)
	str r1,[r0,#VectCtrl0]

	; Clear any previous interrupt
	mov r1,#0
	str r1,[r0,#VectAddr]

	; Enable channel 4 (TIMER0) interrupts
	mov r1, #(1 << 4)
	str r1,[r0,#IntEnable]

	; === Main Program ===
	
	; Memory addresses we will be using
	ldr r4,=time
	ldr r5,=(GPIO1 + PINS)
	
	; Hours and minutes counters
	mov r6,#0 ; Hours
	mov r7,#0 ; Minutes

	; Initialize the seccond counter stored in memory to 0
	str r6,[r4]

main_loop

	; Retrieve the number of seconds from the seconds counter stored in memory
	ldr r8,[r4]
	
	; Add 1 to the minute counter if the seconds counter is over 60
	subs r0,r8,#60
	blt sec_endIf
	mov r8,r0
	str r8,[r4] ; Remember to update our value in memory!
	add r7,r7,#1
sec_endIf

	; Add 1 to the hour counter if the minute counter is over 60
	subs r0,r7,#60
	blt min_endIf
	mov r7,r0
	add r6,r6,#1
min_endIf

	; Reset the hour counter if more than 99 hours have passed
	cmp r6,#99
	ble hour_endIf
	mov r6,#0
hour_endIf

	; Convert hours to BCD, prepend to new output value
	mov r0,r6
	bl div10
	orr r0,r0,r1,lsl #4
	orr r9,r0,r9,lsl #8

	; Prepend '1111' separator bits to new value
	lsl r9,r9,#4
	orr r9,r9,#0xF

	; Convert minutes to BCD, prepend to new output value
	mov r0,r7
	bl div10
	orr r0,r0,r1,lsl #4
	orr r9,r0,r9,lsl #8

	; Prepend '1111' separator bits to new value
	lsl r9,r9,#4
	orr r9,r9,#0xF
	
	; Convert seconds to BCD, prepend to new output value
	mov r0,r8
	bl div10
	orr r0,r0,r1,lsl #4
	orr r9,r0,r9,lsl #8

	; Write new value to GPIO1 output pins
	str r9,[r5]
	
	; It would be effective to use a sleep instruction like 'wfi'
	; here, but, as far as I know, there doesn't appear to be a
	; similar instruction available on this CPU.

	; Repeat forever...
	b main_loop

; Divide by 10 subroutine
; The quotient is found by counting the number of times
; the divisor can be subtracted by 10 before the value
; becomes negative. The remainder is this negitive value
; plus 10.
; Params:
;  r0 - The divisor
; Returns:
;  r0 - The remainder of the division
;  r1 - The quotient of the division
div10	mov r1,#-1      ; q = -1;
div10_l	add r1,r1,#1    ; do:
	subs r0,r0,#10  ;   q++ ; r -= 10
	bge div10_l     ; while r >= 0
	add r0,r0,#10   ; r += 10
	bx lr           ; return r, q

; Interrupt ReQuest (IRQ) Exception Handler
; This handler simply adds 1 to the time counter stored
; in memory. This value can then be read from main.
IRQHandler

	; Save program state, adjusting LR for IRQs by subtracting 4
	sub lr,lr,#4
	stmfd sp!,{r0-r1,lr}
	
	; Add 1 to the timer counter
	ldr r0,=time
	ldr r1,[r0]
	add r1,r1,#1
	str r1,[r0]

	; Reset TIMER0 interrupt by writing 0x01 to T0IR
	ldr r0,=TIMER0
	mov r1,#0x01
	str r1,[r0,#IR]

	; Clear source of interrupt by writing 0 to VICVectAddr
	ldr r0,=VIC
	mov r1,#0
	str r1,[r0,#VectAddr]

	; Restore registers, PC (from LR) and CPSR
	ldmfd sp!,{r0-r1,pc}^

	area tcddata,data,readwrite
	
	; Reserve enough space for our seconds counter.
	; This counter is incremented once every second
	; by the IRQ handler. The counter is reset after
	; it reaches 60, which then adds 1 to the minute
	; counter, etc...
time	space 4

	end