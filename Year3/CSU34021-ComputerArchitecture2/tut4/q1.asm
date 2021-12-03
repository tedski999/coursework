
;
; q1.asm - Ted Johnson (TCD 19335618), 2021
; Contains DLX/MIPS assembly code
;

; Part 1
; See p1.png for screenshot

ADD  R1 R0 R0
NOP  -- -- --
ADD  R0 R1 R0
HALT -- -- --

; Part 2
; See p2.png for screenshot

ADD  R1 R0 R0
ADD  R2 R1 R0
ADD  R0 R2 R1
HALT -- -- --

; Part 3
; See p3.png for screenshot

ADD  R1 R0 R0
ST   R1 R0 00
HALT

; Part 4
; See p4.png for screenshot

J    -- -- 00

; Part 5
; See p5.png for screenshot

ADD  R1 R0 R0
ADD  R0 R0 R0 ; NOTE: A NOP instruction here caused a bug in the animation to not zero-detect mux 9
BEQZ -- R1 F8

; Part 6
; See p6.png for screenshot

J    -- -- 08
HALT -- -- --
J    -- -- F8
