#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "listset.h"

#define true  1
#define false 0

static struct listnode *listset_internal_createNode(char *item, struct listnode *next);
static void listset_internal_prepend(struct listset *set, char *item);
static void listset_internal_appendUnique(struct listset *set, char *item);

// create a new, empty linked listset
struct listset *listset_new(void) {
	struct listset *set = calloc(1, sizeof *set); // allocate AND initialize with zeros
	if (!set) {
		fprintf(stderr, "mem alloc failure");
		return NULL;
	}

	return set;
}

// check to see if an item is in the set, returns 1 if in the listset, 0 if not
int listset_lookup(struct listset *set, char *item) {
	if (!set)
		return false;

	for (struct listnode *cur_node = set->head; cur_node; cur_node = cur_node->next)
		if (strcmp(cur_node->str, item) == 0)
			return true;

	return false;
}

// add an item, with number 'item' to the start of the listset, has no effect if the item is already in the listset
void listset_add(struct listset *set, char *item) {
	if (!set || listset_lookup(set, item))
		return;
	listset_internal_prepend(set, item);
}

// remove an item with number 'item' from the listset
void listset_remove(struct listset *set, char *item) {
	if (!set)
		return;

	struct listnode *prev_node = NULL, *cur_node = set->head;
	while (cur_node) {
		if (strcmp(cur_node->str, item) == 0) {
			if (prev_node)
				prev_node->next = cur_node->next;
			else
				set->head = cur_node->next;
			free(cur_node);
			break;
		}

		prev_node = cur_node;
		cur_node = cur_node->next;
	}
}

// clears all elements from a listset
void listset_clear(struct listset *set) {
	if (!set)
		return;

	struct listnode *cur_node = set->head;
	while (cur_node) {
		struct listnode *next_node = cur_node->next;
		free(cur_node);
		cur_node = next_node;
	}

	set->head = NULL;
}

// place a copy of src into the end of dst, maintaining order of any added elements
void listset_copy(struct listset *dst, struct listset *src) {
	if (!dst || !src || dst == src)
		return;

	for (struct listnode *src_node = src->head; src_node; src_node = src_node->next)
		listset_internal_appendUnique(dst, src_node->str);
}

// place the union of src1 and src2 into the end of dest, maintaining order of any added elements
void listset_union(struct listset *dst, struct listset *src1, struct listset *src2) {
	if (!dst || !src1 || !src2)
		return;

	listset_copy(dst, src1);
	listset_copy(dst, src2);
}

// place the intersection of src1 and src2 into the end of dest, order is not preserved
void listset_intersect(struct listset *dst, struct listset *src1, struct listset *src2) {
	if (!dst || !src1 || !src2)
		return;

	for (struct listnode *src1_node = src1->head; src1_node; src1_node = src1_node->next)
		for (struct listnode *src2_node = src2->head; src2_node; src2_node = src2_node->next)
			if (strcmp(src1_node->str, src2_node->str) == 0)
				listset_internal_appendUnique(dst, src2_node->str);
}

// return the number of items in the listset
int listset_cardinality(struct listset *set) {
	if (!set)
		return -1;

	int count = 0;
	for (struct listnode *cur_node = set->head; cur_node; cur_node = cur_node->next)
		count++;

	return count;
}

// print the elements of the listset
void listset_print(struct listset * this) {
	struct listnode *p;
	for (p = this->head; p != NULL; p = p->next)
		printf("%s, ", p->str);
	printf("\n");
}

// delete allocated memory for listset
void listset_destory(struct listset *set) {
	if (!set)
		return;
	listset_clear(set);
	free(set);
}

// allocates and initializes a new listnode
// NOTE: arguments are assumed to be valid!
static struct listnode *listset_internal_createNode(char *item, struct listnode *next) {
	struct listnode *new_node = malloc(sizeof *new_node);
	if (!new_node) {
		fprintf(stderr, "mem alloc failure");
		return NULL;
	}
	*new_node = (struct listnode) { item, next };
	return new_node;
}

// adds a new node to the beginning of the listset
// NOTE: this function does not check for duplicates! arguments are assumed to be valid!
static void listset_internal_prepend(struct listset *set, char *item) {
	struct listnode *new_node = listset_internal_createNode(item, set->head);
	set->head = new_node;
}

// if its unique, adds a new node to the end of the listset
// NOTE: arguments are assumed to be valid!
static void listset_internal_appendUnique(struct listset *set, char *item) {
	struct listnode *prev_node = NULL, *cur_node = set->head;
	if (!cur_node) {
		listset_internal_prepend(set, item);
	} else {
		do {
			if (strcmp(cur_node->str, item) == 0)
				return;
			prev_node = cur_node;
		} while ((cur_node = cur_node->next));
		prev_node->next = listset_internal_createNode(item, NULL); // here, prev_node is currently the last node in the listset
	}
}


