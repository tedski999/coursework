all: playing_cards.h playing_cards.c playing_cards_main.c
	gcc -o cards -O0 -Wall -g playing_cards.c playing_cards_main.c

test: all
	./cards
