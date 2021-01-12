#ifndef LIST_STRING_H
#define LIST_STRING_H

// a node in a linked list string
struct ls_node {
	char c; // one character of the string
	struct ls_node *next;
};

// the list string type that points to the nodes
struct list_string {
	struct ls_node *head; // pointer to the first node of the string
};

// create a new list string with the same value as a normal
// NULL-terminated C string
struct list_string *new_list_string(char *text);

// find the length of the list string
int list_string_length(struct list_string *ls);

// compare two strings; return -1 is s1 is lexicographically less than s2;
// return 0 if the strings are equal; return 1 if s1 is lexicographically
// larger than s2. E.g. "aB" is less than "ab" because 'B' has a smaller
// ASCII code than 'b'. Also "abc" is less that "abcd". 
int list_string_compare(struct list_string *s1, struct list_string *s2);

// return 1 if str is a substring of text; 0 otherwise
int list_string_substring(struct list_string *text, struct list_string *str);

// deallocate list_string memory
void list_string_destroy(struct list_string *ls);

#endif

