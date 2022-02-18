all: lab1-code.c lab1-code.h lab1-main.c
	gcc -msse4 -O2 -o vec lab1-code.c lab1-main.c

debug: lab1-code.c lab1-code.h lab1-main.c
	gcc -msse4 -g -o vec lab1-code.c lab1-main.c

test: all
	./vec
