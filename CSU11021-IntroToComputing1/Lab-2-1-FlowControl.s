;
; CSU11021 Introduction to Computing I 2019/2020
; Flow Control
;

	AREA	RESET, CODE, READONLY
	ENTRY

; (i)
; if (h >= 13) {
; 	h = h - 12;
; }


; (ii)
; if (a > b) {
;	i = i + 1;
; } else {
;	i = i - 1;
; }


; (iii)
; if (v < 10) {
; 	a = 1;
; }
; else if (v < 100) {
; 	a = 10;
; }
; else if (v < 1000) {
; 	a = 100;
; }
; else {
; 	a = 0;
; }


; (iv)
; i = 3;
; while (i < 1000) {
; 	a = a + i;
; 	i = i + 3;
; }


; (v) 
; for (int i = 3; i < 1000; i = i + 3) {
; 	a = a + i;
; }


; (vi)
; p = 1;
; do {
; 	p = p * 10;
; } while (v < p);


; (vii)
; if (ch >= 'A' && ch <= 'Z') {
; 	upper = upper + 1;
; }


; (viii)
; if (ch=='a' || ch=='e' || ch=='i' || ch=='o' || ch=='u')
; {
; 	v = v + 1;
; }


STOP	B	STOP

	END
