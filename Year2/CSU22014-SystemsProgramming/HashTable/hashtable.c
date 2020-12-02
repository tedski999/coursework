#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "hashtable.h"

// compute a hash of a string using a seed value,
// where the result falls between zero and range - 1
int hash_string(char *string, int range) {
	int hash = 0;
	const int HASH_SEED = 19;

	for (int i = 0; string[i] != '\0'; i++)
		hash = hash * HASH_SEED + string[i];
	if (hash < 0)
		hash = -hash;

	return hash % range;
}

// create a new empty hashtable
struct hashtable *hashtable_new(int size) {
	struct hashtable *new_table = malloc(sizeof *new_table);
	if (!new_table) {
		fprintf(stderr, "mem alloc failure");
		return NULL;
	}

	new_table->size = size;
	new_table->table = calloc(new_table->size, sizeof *new_table->table);
	if (!new_table->table) {
		fprintf(stderr, "mem alloc failure");
		free(new_table);
		return NULL;
	}

	return new_table;
}

// add an item to the hashtable
void hashtable_add(struct hashtable *table, char *item) {
	if (!table)
		return;
	int hash = hash_string(item, table->size);
	listset_add(table->table + hash, item);
}

// return 1 if item is in hashtable, 0 otherwise
int hashtable_lookup(struct hashtable *table, char *item) {
	if (!table)
		return 0;
	int hash = hash_string(item, table->size);
	return listset_lookup(table->table + hash, item);
}

// remove an item from the hash table; if the item is in the table
// multiple times, just remove the first one that we encounter
void hashtable_remove(struct hashtable *table, char *item) {
	int hash = hash_string(item, table->size);
	listset_remove(table->table + hash, item);
}

// print the elements of the hashtable set
void hashtable_print(struct hashtable *table) {
	if (!table)
		return;
	for (int i = 0; i < table->size; i++)
		listset_print(table->table + i);
}

// clears all elements from a hashtable
void hashtable_clear(struct hashtable *table) {
	if (!table)
		return;
	for (int i = 0; i < table->size; i++)
		listset_clear(table->table +i);
}

// delete allocated memory for the hashtable
void hashtable_destroy(struct hashtable *table) {
	if (!table)
		return;
	hashtable_clear(table);
	free(table->table);
}

