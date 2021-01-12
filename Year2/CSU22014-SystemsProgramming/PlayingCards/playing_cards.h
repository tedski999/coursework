#ifndef PLAYING_CARDS_H
#define PLAYING_CARDS_H

#define N_CARD_SUITS 4
#define LOW_CARD_VALUE 1
#define HIGH_CARD_VALUE 13

struct playing_card {
  int suit;  // value in the range 0..3 inclusive
  int value; // value in the range 1..13 inclusive
};

// pack the playing card structures into bytes of memory
unsigned char *pack_playing_cards(struct playing_card *cards, int number_of_cards);

// unpack bytes of memory containing card data into playing card structures
struct playing_card *unpack_playing_cards(unsigned char *packed_cards, int number_of_cards);

#endif

