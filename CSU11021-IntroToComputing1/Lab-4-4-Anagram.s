;
; CSU11021 Introduction to Computing I 2019/2020
; Anagrams
;

; R0 - Result
; R1 - Primes memory location
; R2 - scoreStr parameter (string memory location)
; R3 - scoreStr output (string score)
; R4 - temporary storage for string1 score

	AREA	RESET, CODE, READONLY
	ENTRY
	
	LDR	R1, =primes	; get memory location of primes
	
	LDR	R2, =tststr1	; get memory location of string1
	BL	scoreStr	; compute score of string1
	MOV	R4, R3		; move string1 score to different reg
	
	LDR	R2, =tststr2	; get memory location of string2
	BL	scoreStr	; compute score of string2
	
	CMP	R3, R4		; set result in R0
	MOVEQ	R0, #1		; 1 if they're anagram
	MOVNE	R0, #0		; 0 if they're not anagram

STOP	B	STOP

; Compute score of string using prime factorization
scoreStr
	PUSH	{LR,R0}		; save previous regs
	LDR	R3, =1		; initial string score
	
whl	LDRB	R0, [R2]	; load char from memory
	CMP	R0, #0		; loop through entire string
	BNE	eIf		; if at end of string:
	POP	{PC,R0}		;   return from subroutine
eIf	CMP	R0, #' '	; if char is a space
	BEQ	sSpace		;   don't include it in the score
	
	AND	R0, R0, #0x1F	; convert character to a memory offset
	SUB	R0, R0, #1	; ^
	MOV	R0, R0, LSL #2	; ^
	ADD	R0, R0, R1	; compute memory location of prime
	LDR	R0, [R0]	; load prime from memory location
	MUL	R3, R0, R3	; multiply current score
sSpace
	ADD	R2, R2, #1	; move to next char
	B	whl		; branch back to top of subrountine

tststr1	DCB	"ta pad",0
tststr2	DCB	"pa sta",0
primes	DCD	2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101

	END
