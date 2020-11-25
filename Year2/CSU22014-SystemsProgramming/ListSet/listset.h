#ifndef LISTSET_LIST_H
#define LISTSET_LIST_H

// a linked list node containing a string
struct listnode {
	char *str;
	struct listnode *next;
};

// a linked list data structure
struct listset {
	struct listnode *head;
};

struct listset *listset_new(void);                    // create a new, empty linked list set
int listset_lookup(struct listset *set, char *item);  // check to see if an item is in the set, returns 1 if in the set, 0 if not
void listset_add(struct listset *set, char *item);    // add an item, with number 'item' to the start of the set, has no effect if the item is already in the set
void listset_remove(struct listset *set, char *item); // remove an item with number 'item' from the set
void listset_copy(struct listset *dst, struct listset *src);                              // place a copy of src into dst
void listset_union(struct listset *dst, struct listset *src1, struct listset *src2);      // place the union of src1 and src2 into dst
void listset_intersect(struct listset *dst, struct listset *src1, struct listset *src2);  // place the intersection of src1 and src2 into dst
int listset_cardinality(struct listset *set);         // return the number of items in the listset
void listset_print(struct listset *set);              // print the elements of the list set
void listset_destory(struct listset *set);            // delete allocated memory for set

#endif

