
;
; t2.asm - Ted Johnson (TCD 19335618), 2021
; Contains implementation for tut2 functions written in x64 Assembly with NASM syntax (GCC calling convention)
;

bits 64
default rel

extern scanf
extern printf

global inp_int
global gcd_recursion
global use_scanf
global min
global min5


section .data

inp_int: dq 0
prompt: db "Please enter an integer: ", 0
format: db "%lld", 0
output: db "The sum of the maximum value and user input (%lld, %lld): %lld", 0xa, 0


section .text

	; gcd_recursion - Find the GCD of a and b using recursion
	; Parameters: (int64) a, (int64) b
	; Returns: (int64)
	; Side-effects: None
gcd_recursion:
	mov rax, rdi       ; rax = a
	cmp rsi, 0         ; if (b != 0)
	je .base_case      ; {
	mov rdi, rsi       ;   a = b
	mov rdx, 0         ;   r = rax % b;
	idiv rsi           ;   ...
	mov rsi, rdx       ;   gcd_recursion(b, r);
	call gcd_recursion ;   ...
.base_case:            ; }
	ret

	; use_scanf - Print and return the sum of the max value in arr and inputted global variable inp_int
	; Parameters: (int64) arr_size, (int64 *) arr
	; Returns: (int64)
	; Side-effects: None
use_scanf:
	push rbx

	; Find max value in arr
	mov rbx, 1<<63        ; max = INT64_MIN
	lea rdx, [rsi+rdi*8]  ; arr_end = arr[arr_size]
.foreach:                 ;
	cmp rsi, rdx          ; while (arr < arr_end)
	jge .endforeach       ; {
	mov rcx, [rsi]        ;   item = *arr
	cmp rcx, rbx          ;   if (item > max)
	cmovg rbx, rcx        ;     max = item
	add rsi, 8            ;   arr++
	jmp .foreach          ; }
.endforeach:

	; Read value from scanf
	mov rdi, prompt       ; printf("Please enter an integer: ")
	call printf wrt ..plt ; ...
	mov rdi, format       ; scanf("%lld", inp_int)
	mov rsi, inp_int      ; ...
	call scanf wrt ..plt  ; ...

	; Print sum of max value and input int
	mov rdi, output       ; printf("The sum of the maximum value and user input (%lld, %lld): %lld\n", max, *inp_int, *inp_int + max)
	mov rsi, rbx          ; ...
	mov rdx, [inp_int]    ; ...
	mov rcx, rdx          ; ...
	add rcx, rbx          ; ...
	call printf wrt ..plt ; ...

	mov rax, [inp_int]    ; rax = *inp_int + max
	add rax, rbx          ; ...
	pop rbx
	ret

	; min - Find the minimum of a, b and c
	; Parameters: (int64) a, (int64) b, (int64) c
	; Returns: (int64)
	; Side-effects: None
min:
	mov rax, rdi       ; rax = a
	cmp rsi, rax       ; if (b < rax)
	cmovl rax, rsi     ;   rax = b
	cmp rdx, rax       ; if (c < rax)
	cmovl rax, rdx     ;   rax = c
	ret

	; min5 - Find the minimum of i, j, k, l and global variable inp_int
	; Parameters: (int64) i, (int64) j, (int64) k, (int64) l
	; Returns: (int64)
	; Side-effects: None
min5:
	sub rsp, 8
	call min           ; rax = min(i, j, k)
	mov rdi, rax       ; rax = min(rax, l, *inp_int)
	mov rsi, rcx       ; ...
	mov rdx, [inp_int] ; ...
	call min           ; ...
	add rsp, 8
	ret
