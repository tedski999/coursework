#include "bitset.h"
#include <stdlib.h>

static const int WORD_SIZE = 8 * sizeof (int64_t); // bit size of each element in bitset.bits array (should be 64)

// create a new, empty bit vector set with a universe of 'size' items
struct bitset *bitset_new(int size) {
	struct bitset *new_bitset = malloc(sizeof *new_bitset);
	new_bitset->universe_size = size;
	new_bitset->size_in_words = (size - 1) / WORD_SIZE + 1;
	new_bitset->bits = calloc(new_bitset->size_in_words, WORD_SIZE);
	return new_bitset;
}

// get the size of the universe of items that could be stored in the set
int bitset_size(struct bitset *bitset) {
	return bitset->universe_size;
}

// get the number of items that are stored in the set
// this uses an implementation of Brian Kernighan's bit counting algorithm
int bitset_cardinality(struct bitset *bitset) {
	int count = 0;
	for (int i = 0; i < bitset->size_in_words; i++) {
		int64_t word = bitset->bits[i];
		while (word) {
			word &= (word - 1);
			count++;
		}
	}

	return count;
}

// check to see if an item is in the set
int bitset_lookup(struct bitset *bitset, int item) {
	if (item < bitset->universe_size && bitset->bits[item / WORD_SIZE] & (1UL << (item % WORD_SIZE)))
		return 1;
	return 0;
}

// add an item, with number 'item' to the set
// has no effect if the item is already in the set
void bitset_add(struct bitset *bitset, int item) {
	bitset->bits[item / WORD_SIZE] |= 1UL << (item % WORD_SIZE);
}

// remove an item with number 'item' from the set
void bitset_remove(struct bitset *bitset, int item) {
	bitset->bits[item / WORD_SIZE] &= ~(1UL << (item % WORD_SIZE));
}

// place the union of src1 and src2 into dest;
// all of src1, src2, and dest must have the same size universe
void bitset_union(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
	if (src1->universe_size != src2->universe_size)
		return;

	free(dest);
	dest = bitset_new(src1->universe_size);
	if (dest) {
		for (int i = 0; i < dest->size_in_words; i++)
			dest->bits[i] = src1->bits[i] | src2->bits[i];
	}
}

// place the intersection of src1 and src2 into dest
// all of src1, src2, and dest must have the same size universe
void bitset_intersect(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
	if (src1->universe_size != src2->universe_size)
		return;

	free(dest);
	dest = bitset_new(src1->universe_size);
	if (dest) {
		for (int i = 0; i < dest->size_in_words; i++)
			dest->bits[i] = src1->bits[i] & src2->bits[i];
	}
}
