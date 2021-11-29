
;
; q1.asm - Ted Johnson (TCD 19335618), 2021
; Contains RISC-1 assembly code
;

; min(i, j, k) unoptimized
; Parameters i-k are assumed to be stored in r26-r28
; 9 - 11 clock cycles
min:
	add r0, r26, r1      ; v = i
	sub r27, r1, r0 {c}  ; if j < v:
	jgt .i               ;
	nop                  ;   nop
	add r0, r27, r1      ;   v = j
.i:	sub r28, r1, r0 {c}  ; if k < v:
	jgt .j               ;
	nop                  ;   nop
	add r0, r28, r1      ;   v = k
.j:	ret r31              ; return v
	nop                  ; nop


; min(i, j, k) optimized
; Parameters i-k are assumed to be stored in r26-r28
; 7 - 9 instructions executed
min:
	sub r27, r26, r0 {c} ; check if j < i
	jge .i               ;
	add r0, r26, r1      ; set v = i
	add r0, r27, r1      ; if j < i: v = j
.i:	sub r28, r1, r0 {c}  ; check if k < v
	jgt .j               ;
	xor r0, r0, r0       ; nop
	add r0, r28, r1      ; if k < v: v = k
.j:	ret r31              ; return v


; min(i, j, k) optimized with jmp/ret hack
; Parameters i-k are assumed to be stored in r26-r28
; 6 - 8 clock cycles
; Note: This subroutine assumes a delayed ret instruction occurring
; directly after a delayed jmp instruction will allow for a single
; non-delayed instruction to be executed before the delayed ret is
; executed. For example:
;    jmp l
;    ret r32
; l: add r0, r2, r1
; Here, a jmp is encountered. During the jmp 'delay slot', a ret
; instruction is encountered. During the ret 'delay slot', an add
; instruction is executed. Finally the ret instruction is executed.
min:
	sub r27, r26, r0 {c} ; check if j < i
	jge .i               ;
	add r0, t26, r1      ; set v = i
	add r0, t27, r1      ; if j < i: v = j
.i:	sub r28, r1, r0 {c}  ; check if k < v
	jgt .j               ;
	ret r31              ; either way, return v
.j:	add r0, r28, r1      ; if k < v: v = k


; min5(i, j, k, l)
; Parameters i-l are assumed to be stored in r26-r29
; Global variable inp_int is assumed to be stored in r9
; 10 clock cycles + 2 min calls
min5:
	add r0, r26, r11     ; param i
	add r0, r27, r12     ; param j
	call min, r15        ; v = min(inp_int, i, j)
	add r0, r9, r10      ; param inp_int
	                     ;
	add r0, r28, r11     ; param k
	add r0, r29, r12     ; param l
	call min, r15        ; v = min(v, k, l)
	add r0, r1, r10      ; param v
	                     ;
	ret r31              ; return v
	xor r0, r0, r0       ; nop
