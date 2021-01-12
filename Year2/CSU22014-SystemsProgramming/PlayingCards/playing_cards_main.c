//   playing_cards_main.c
//   David Gregg
//   December 2020

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "playing_cards.h"

// create a complete pack of cards
struct playing_card * create_pack_cards(void) {
  // create an array of all playing cards
  struct playing_card * result;
  int ncard_values = HIGH_CARD_VALUE - LOW_CARD_VALUE + 1;
  int ncards = ncard_values * N_CARD_SUITS;
  result = malloc(sizeof(struct playing_card*) * ncards);
  assert( result != NULL );
  
  // populate the array with all cards
  int card_num = 0;
  for ( int i = 0; i < N_CARD_SUITS; i++ ) {
    for ( int j = LOW_CARD_VALUE; j <= HIGH_CARD_VALUE; j++ ) {
      result[card_num].suit = i;
      result[card_num].value = j;
      card_num++;
      //fprintf(stderr, "suit %d, value %d\n", i, j);
    }
  }
  return result;
}

int main() {
  // keep track of the number of errors encountered
  int nerrors = 0;
  
  // create an array of all playing cards
  struct playing_card * all_cards = create_pack_cards();

  // pack the playing cards
  int ncard_values = HIGH_CARD_VALUE - LOW_CARD_VALUE + 1;
  int ncards = ncard_values * N_CARD_SUITS;
  unsigned char * packed = pack_playing_cards(all_cards, ncards);
  assert( ncards == 52 );
  
  // check the array of packed values
  int nbytes = 39;
  unsigned char correct_packed[] = {0x4, 0x20, 0xc4, 0x14, 0x61, 0xc8, 0x24, 0xa2, 0xcc, 0x35, 0x14, 0x93, 0x51, 0x55, 0x97, 0x61, 0x96, 0x9b, 0x71, 0xd8, 0x62, 0x8e, 0x49, 0x66, 0x9e, 0x8a, 0x6a, 0xae, 0xcb, 0x71, 0xcb, 0x3d, 0x35, 0xdb, 0x7e, 0x39, 0xeb, 0xbf, 0x3d};
  for ( int i = 0; i < nbytes; i++ ) {
    if ( packed[i] != correct_packed[i] ) {
      fprintf(stderr,
	      "Error: Bad match between packed[%d]:%x and correct[%d]:%x\n",
	      i, packed[i], i, correct_packed[i]);
      nerrors++;
    }
  }
  fprintf(stderr, "%d errors encountered while packing\n", nerrors);

  //unpack the playing cards
  struct playing_card * unpacked;
  unpacked = unpack_playing_cards(packed, ncards);

  // check that the result of unpacking is the same as the original
  nerrors = 0;
  for ( int i = 0; i < ncards; i++ ) {
    if ( (unpacked[i].suit != all_cards[i].suit)
      || (unpacked[i].value != all_cards[i].value) ) {
      fprintf(stderr, "Error: Bad match between %d, %d and %d, %d\n",
	      unpacked[i].suit, unpacked[i].value,
	      all_cards[i].suit, all_cards[i].value);
      nerrors++;
    }
  }
  fprintf(stderr, "%d errors encountered while unpacking\n", nerrors);
  return 0;
}
