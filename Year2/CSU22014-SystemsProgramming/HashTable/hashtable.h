#ifndef HASHTABLE_HASHTABLE_H
#define HASHTABLE_HASHTABLE_H

#include "listset.h"

// a hash table data structure
struct hashtable {
	struct listset *table;
	int size;
};

struct hashtable *hashtable_new(int size);                  // create a new, empty hashtable set
int hashtable_lookup(struct hashtable *table, char *item);  // check to see if an item is in the set, returns 1 if in the set, 0 if not
void hashtable_add(struct hashtable *table, char *item);    // add an item, with number 'item' to the start of the hashset entry set
void hashtable_remove(struct hashtable *table, char *item); // remove an item with number 'item' from the hastable set
int hashtable_cardinality(struct hashtable *table);         // return the number of items in the hashtable set
void hashtable_clear(struct hashtable *table);              // clear all entries from hashtable
void hashtable_print(struct hashtable *table);              // print the elements of the hashtable set

#endif

