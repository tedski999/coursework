
	; poly - Calculate x^3 + x^2 + x + 1
	; Parameters: (int32) x
	; Returns: (int32) The result of x^3 + x^2 + x + 1
	; Side-effects: None
	global poly
poly:
	push ebp
	mov ebp, esp
	push ebx

	; compute x + 1
	mov ebx, 1       ; ebx = 1
	add ebx, [ebp+8] ; ebx += x
	                 ;
	; add x^2        ;
	push 2           ; eax = pow(x, 2)
	mov eax, [ebp+8] ; ...
	push eax         ; ...
	call pow         ; ...
	add esp, 8       ; ...
	add ebx, eax     ; ebx += eax
	                 ;
	; add x^3        ;
	push 3           ; eax = pow(x, 3)
	mov eax, [ebp+8] ; ...
	push eax         ; ...
	call pow         ; ...
	add esp, 8       ; ...
	add eax, ebx     ; eax += ebx

	; return
	pop ebx
	mov esp, ebp
	pop ebp
	ret

	; pow (private) - Calculate x^y
	; Parameters: (int32) x, (int32) y
	; Returns: (int32) The result of x^y
	; Side-effects: None
pow:
	push ebp
	mov ebp, esp

	; compute x^y and write to eax
	mov eax, 1        ; eax = 1
	mov ecx, 1        ; ecx = 1
pow_loop:             ;
	cmp ecx, [ebp+12] ; while (ecx <= y)
	jg pow_end        ; {
	imul eax, [ebp+8] ;   eax *= x
	inc ecx           ;   ecx++
	jmp pow_loop      ; }
pow_end:

	; return
	mov esp, ebp
	pop ebp
	ret

	; factorial - Calculate the factorial of x
	; Parameters: (int32) x
	; Returns: (int32) factorial of x
	; Side-effects: None
	global factorial
factorial:
	push ebp
	mov ebp, esp

	; compute factorial of x and write to eax
	mov eax, [ebp+8]  ; eax = x
	cmp eax, 1        ; if (eax != 1)
	je factorial_end  ; {
	dec eax           ;   eax--
	push eax          ;   eax = factorial(eax)
	call factorial    ;   ...
	add esp, 4        ;   ...
	imul eax, [ebp+8] ;   eax *= x
factorial_end:            ; }

	; return
	mov esp, ebp
	pop ebp
	ret

	; multiple_k_asm - Where m <= i < n, set arr[i] to 1 if i is divisible by k, otherwise set arr[i] to 0
	; Parameters: (uint16) m, (uint16) n, (uint16) k, (uint16 *) arr
	; Returns: None
	; Side-effects: arr[m:n] is modified
	global multiple_k_asm
multiple_k_asm:
	push ebp
	mov ebp, esp
	push ebx

	; while ebx < n
	mov ebx, [ebp+8]        ; ebx = m
	mov ecx, [ebp+20]       ; ecx = arr
multiple_k_asm_loop:            ;
	cmp bx, [ebp+12]        ; while (bx < n)
	jge multiple_k_asm_end  ; {
	                        ;
	; divide bx by k        ;
	mov ax, bx              ;   dx = bx % k
	mov dx, 0               ;   ...
	div word [ebp+16]       ;   ...
	                        ;
	; if remainder == 0,    ;
	; set arr[ebx] to 1     ;
	cmp dx, 0               ;   if (dx == 0)
	sete [ecx + ebx * 2]    ;   { arr[ebx] = 1 } else { arr[ebx] = 1 }
	                        ;
	; continue while loop   ;
	inc bx                  ;   bx++
	jmp multiple_k_asm_loop ; }
multiple_k_asm_end:

	; return
	pop ebx
	mov esp, ebp
	pop ebp
	ret
