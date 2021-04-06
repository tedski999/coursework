; This program polls GPIO1 pins 27-24 for active-low input. The value of D is outputted to GPIO1 pins 23-16 as an 8 bit number.
; Input pin 24 increments D, input pin 25 decrements D, input pin 26 bit-shifts D left and input pin 27 bit-shifts D right.
; This program does not use sub-routines as I found them unnecessary and only proved to make the program harder to follow.
; Generally, r0 is used to store the address of GPIO1 pin register, r1 contains the GPIO1 pin values excluding pins 23-16
; while the value of D is written to and modified within r2 before being merged with r1 and written to the address in r0.
; Made with love, indented with 8-spaced tabs.
; Ted johnson, 2021.

	area tcd,code,readonly
	export __main

__main

IO1DIR	equ 0xE0028018
IO1PIN	equ 0xE0028010

	; Set the appropriate GPIO1 pin directions
	ldr r0,=IO1DIR                                  ; dirs_addr = DIRS_ADDR
	ldr r1,[r0]                                     ; dirs = loadReg(dirs_addr)
	bic r1,r1,#0x0F000000                           ; clr bits 27-24 in dirs
	orr r1,r1,#0x00FF0000                           ; set bits 23-16 in dirs
	str r1,[r0]                                     ; storeReg(dirs, dirs_addr)
	
	; Initialize D to 0
	ldr r0,=IO1PIN                                  ; pins_addr = PINS_ADDR
	ldr r1,[r0]                                     ; pins = loadReg(pins_addr)
	bic r1,r1,#0x00FF0000                           ; clr bits 23-16 in pins
	str r1,[r0]                                     ; storeReg(pins, pins_addr)

	; Block while all button input pins are high
polling	ldr r1,[r0]                                     ; pins = loadReg(pins_addr)
	eor r2,r1,#0x0F000000                           ; temp = pins; not bits 27-24 in temp
	tst r2,#0x0F000000                              ; if bits 27-24 are set in temp:
	beq polling                                     ;   goto polling
	
	; Isolate the value of D from the GPIO1 pins
	and r2,r1,#(0xFF << 16)                         ; d = pins; clr all bits except 23-16 in d

	; If button 24 is pressed: D = D + 1
	; (We add 2^16 to account for bit offset)
	tst r1,#(1 << 24)                               ; if bit 24 is set in pins:
	addeq r2,r2,#(1 << 16)                          ;   d = d + 2^16
	beq write_d                                     ;   goto write_d

	; If button 25 is pressed: D = D - 1
	; (We subtract 2^16 to account for bit offset)
	tst r1,#(1 << 25)                               ; if bit 25 is set in pins:
	subeq r2,r2,#(1 << 16)                          ;   d = d - 2^16
	beq write_d                                     ;   goto write_d

	; If button 26 is pressed: D = D << 1
	tst r1,#(1 << 26)                               ; if bit 26 is set in pins:
	moveq r2,r2,lsl #1                              ;   d = d << 1
	beq write_d                                     ;   goto write_d

	; Else button 27 is pressed: D = D >> 1
	mov r2,r2,lsr #1                                ; d = d >> 1

	; Merge the new D value with GPIO1 pins
write_d	and r2,r2,#(0xFF << 16)                         ; clr all bits except 23-16 in d
	bic r1,r1,#(0xFF << 16)                         ; clr bits 23-16 in pins
	orr r1,r1,r2                                    ; pins |= d
	str r1,[r0]                                     ; storeReg(pins, pins_addr)

	; Block until all button input pins are high
waiting	ldr r1,[r0]                                     ; pins = loadReg(pins_addr)
	eor r2,r1,#0x0F000000                           ; temp = pins; not bits 27-24 in temp
	tst r2,#0x0F000000                              ; if bits 27-24 are clr in temp:
	bne waiting                                     ;   goto waiting
	
	; Button press complete, return to polling
	b polling                                       ; goto polling

	end