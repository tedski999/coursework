#include "bloom.h"
#include "bitset.h"
#include <stdlib.h>

const int BLOOM_HASH1 = 17;
const int BLOOM_HASH2 = 29;

// compute a hash of a string using a seed value, where the result
// falls between zero and range-1
int hash_string(char *string, int seed, int range) {
	int i, hash = 0;
	for (i = 0; string[i] != '\0'; i++)
		hash = hash * seed + string[i];
	if (hash < 0)
		hash = -hash;
	return hash % range;
}

// create a new, empty Bloom filter of 'size' items
struct bloom *bloom_new(int size) {
	struct bloom *new_bloom = malloc(sizeof *new_bloom);
	if (!new_bloom)
		return NULL;

	*new_bloom = (struct bloom) { bitset_new(size), size };
	if (!new_bloom->bitset) {
		free(new_bloom);
		return NULL;
	}

	return new_bloom;
}

// check to see if a string is in the set
int bloom_lookup(struct bloom *bloom, char *item) {
	int hash1_result = bitset_lookup(bloom->bitset, hash_string(item, BLOOM_HASH1, bloom->size));
	int hash2_result = bitset_lookup(bloom->bitset, hash_string(item, BLOOM_HASH2, bloom->size));
	return hash1_result & hash2_result;
}

// add a string to the set
// has no effect if the item is already in the set
void bloom_add(struct bloom *bloom, char *item) {
	bitset_add(bloom->bitset, hash_string(item, BLOOM_HASH1, bloom->size));
	bitset_add(bloom->bitset, hash_string(item, BLOOM_HASH2, bloom->size));
}

// place the union of src1 and src2 into dest
void bloom_union(struct bloom *dest, struct bloom *src1, struct bloom *src2) {
	if (src1->size != src2->size)
		return;

	free(dest);
	dest = bloom_new(src1->size);
	if (dest)
		bitset_union(dest->bitset, src1->bitset, src2->bitset);
}

// place the intersection of src1 and src2 into dest
void bloom_intersect(struct bloom *dest, struct bloom *src1, struct bloom *src2) {
	if (src1->size != src2->size)
		return;

	free(dest);
	dest = bloom_new(src1->size);
	if (dest)
		bitset_intersect(dest->bitset, src1->bitset, src2->bitset);
}

