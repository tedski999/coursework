#ifndef BLOOMFILTER_BLOOM_H
#define BLOOMFILTER_BLOOM_H

#include "bitset.h"

struct bloom {
  struct bitset *bitset;
  int size;
};

extern const int BLOOM_HASH1;
extern const int BLOOM_HASH2;

struct bloom *bloom_new(int size);                  // create a new, empty Bloom filter of 'size' items
int bloom_size(struct bloom *bloom);                // return size of Bloom filter
int bloom_lookup(struct bloom *bloom, char * item); // check to see if a string is in the set
void bloom_add(struct bloom *bloom, char * item);   // add a string to the set. has no effect if the item is already in the set
// note that you cannot safely remove items from a Bloom filter

void bloom_union(struct bloom *dest, struct bloom *src1, struct bloom *src2);     // place the union of src1 and src2 into dest
void bloom_intersect(struct bloom *dest, struct bloom *src1, struct bloom *src2); // place the intersection of src1 and src2 into dest

#endif

