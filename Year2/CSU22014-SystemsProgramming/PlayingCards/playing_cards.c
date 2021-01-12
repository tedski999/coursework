#include "playing_cards.h"
#include <stdlib.h>
#include <assert.h>

// NOTE: assert() should not actually be used in production

// pack the playing card structures into bytes of memory
unsigned char *pack_playing_cards(struct playing_card *cards, int number_of_cards) {
	assert(cards);

	// The number of bytes needed is the number of bits needed divided by 8 drounded up.
	// To round up, we subtract one before integer division, then add one after.
	const int N_BITS_PER_CARD = 6;
	int size = (number_of_cards * N_BITS_PER_CARD - 1) / 8 + 1;

	// allocate memory for the packed array
	unsigned char *packed_cards = calloc(size, 1);
	assert(packed_cards);

	// iterate over every card we're going to pack into packed_cards
	int packed_cards_index = 0;
	for (int i = 0; i < number_of_cards; i++) {

		// A card can be packed into packed_cards 1 of 4 ways.
		// Every 4 cards are packed differently, then the pattern repeats.
		// To accomplish this, we'll modulo 4 the card index and provide the
		// four different cases in a switch-case statement.
		// NOTE: There's probably a way to do this generically, but I believe
		//       this method has better performance... Plus I don't want to
		//       spend more than an hour on this.
		struct playing_card cur_card = cards[i];
		switch (i % 4) {

			// pack suit into bits 7-6 and value into bits 5-2
			case 0:
				packed_cards[packed_cards_index] |= (cur_card.suit & 0x03) << 6;
				packed_cards[packed_cards_index] |= (cur_card.value & 0x0f) << 2;
				break;

			// pack suit into bits 1-0 and value into bits 7-4 in the next byte
			case 1:
				packed_cards[packed_cards_index] |= cur_card.suit & 0x03;
				packed_cards_index++;
				packed_cards[packed_cards_index] |= (cur_card.value & 0x0f) << 4;
				break;

			// pack suit into bits 3-2 and value into bits 1-0 and 7-6 in the next byte
			case 2:
				packed_cards[packed_cards_index] |= (cur_card.suit & 0x03) << 2;
				packed_cards[packed_cards_index] |= (cur_card.value & 0x0c) >> 2;
				packed_cards_index++;
				packed_cards[packed_cards_index] |= (cur_card.value & 0x03) << 6;
				break;

			// pack suit into bits 5-4 and value into bits 3-0
			case 3:
				packed_cards[packed_cards_index] |= (cur_card.suit & 0x03) << 4;
				packed_cards[packed_cards_index] |= cur_card.value & 0x0f;
				packed_cards_index++;
				break;

			// all i%4 cases should be covered.
			// comment out below if you like living on the edge.
			default:
				assert(0);
		}
	}

	return packed_cards;
}

// unpack bytes of memory containing card data into playing card structures
struct playing_card *unpack_playing_cards(unsigned char *packed_cards, int number_of_cards) {
	assert(packed_cards);

	// allocate memory for the cards array
	struct playing_card *cards = malloc(sizeof *cards * number_of_cards);
	assert(cards);

	// iterate over every card we're going to unpack from packed_cards
	int packed_cards_index = 0;
	for (int i = 0; i < number_of_cards; i++) {

		// This is simply the reverse of the method used in pack_playing_cards().
		// Every 4 cards are unpacked differently, then the pattern repeats.
		// To accomplish this, we'll modulo 4 the card index and provide the
		// four different cases in a switch-case statement.
		int suit, value;
		switch (i % 4) {

			// unpack suit from bits 7-6 and value from bits 5-2
			case 0:
				suit = packed_cards[packed_cards_index] >> 6;
				value = (packed_cards[packed_cards_index] & 0x3c) >> 2;
				break;

			// unpack suit from bits 1-0 and value from bits 7-4 in the next byte
			case 1:
				suit = packed_cards[packed_cards_index] & 0x03;
				packed_cards_index++;
				value = (packed_cards[packed_cards_index] & 0xf0) >> 4;
				break;

			// unpack suit from bits 3-2 and value from bits 1-0 and 7-6 in the next byte
			case 2:
				suit = (packed_cards[packed_cards_index] & 0x0c) >> 2;
				value = (packed_cards[packed_cards_index] & 0x03) << 2;
				packed_cards_index++;
				value |= packed_cards[packed_cards_index] >> 6;
				break;

			// unpack suit from bits 5-4 and value from bits 3-0
			case 3:
				suit = (packed_cards[packed_cards_index] & 0x30) >> 4;
				value = packed_cards[packed_cards_index] & 0x0f;
				packed_cards_index++;
				break;

			// all i%4 cases should be covered.
			// comment out below if you like living on the edge.
			default:
				assert(0);
		}

		// assign the parsed values
		cards[i].suit = suit;
		cards[i].value = value;
	}

	return cards;
}

