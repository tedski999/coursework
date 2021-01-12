#include "list_string.h"
#include <stdlib.h>
#include <assert.h>

// NOTE: assert() should not actually be used in production

// create a new list string with the same value as a normal
// NULL-terminated C string
struct list_string *new_list_string(char *text) {

	// allocate memory for the new list string
	struct list_string *new_ls = malloc(sizeof *new_ls);
	assert(new_ls);

	// return new_ls if theres no need to generate nodes - text has no characters!
	if (!*text) {
		new_ls->head = NULL;
		return new_ls;
	}

	// allocate and initialize memory for the head of the new list string
	struct ls_node *cur_node = malloc(sizeof *cur_node);
	*cur_node = (struct ls_node) { *text, NULL };
	new_ls->head = cur_node;

	// generate a ls_node for each subsequent character in text
	struct ls_node *prev_node = cur_node;
	while (*++text) {
		cur_node = malloc(sizeof *cur_node);
		assert(cur_node);
		*cur_node = (struct ls_node) { *text, NULL };
		prev_node->next = cur_node;
		prev_node = cur_node;
	}
	
	// return the pointer to the allocated memory
	return new_ls;
}

// find the length of the list string
int list_string_length(struct list_string *ls) {
	assert(ls);

	// iterate over the list, counting the number of nodes
	int length = 0;
	struct ls_node *cur_node = ls->head;
	while (cur_node) {
		cur_node = cur_node->next;
		length++;
	}

	// return the number of counted nodes
	return length;
}


// compare two strings; return -1 is s1 is lexicographically less than s2;
// return 0 if the strings are equal; return 1 if s1 is lexicographically
// larger than s2. E.g. "aB" is less than "ab" because 'B' has a smaller
// ASCII code than 'b'. Also "abc" is less that "abcd".
int list_string_compare(struct list_string *s1, struct list_string *s2) {
	assert(s1 && s2);

	// iterate over both linked-lists of nodes, return if a char is different
	struct ls_node *s1_node = s1->head, *s2_node = s2->head;
	while (s1_node && s2_node) {
		if (s1_node->c != s2_node->c)
			return (s1_node->c < s2_node->c) ? -1 : 1;
		s1_node = s1_node->next;
		s2_node = s2_node->next;
	}

	// check if the similar lists are different sizes
	if (s1_node)
		return 1;
	else if (s2_node)
		return -1;

	return 0;
}

// return 1 if str is a substring of text; 0 otherwise
int list_string_substring(struct list_string *text, struct list_string *str) {
	assert(text && str);

	// to locate the str substring, we must iterate over every node in text and
	// check if the str matches all nodes following it
	struct ls_node *text_node = text->head;
	while (text_node) {

		// compare str to the list string starting at text_node
		struct ls_node *cur_text_node = text_node;
		struct ls_node *str_node = str->head;
		while (cur_text_node && str_node) {

			// break if these node don't match
			if (cur_text_node->c != str_node->c)
				break;

			// continue to the next pair of nodes
			cur_text_node = cur_text_node->next;
			str_node = str_node->next;

			// if we're at the end of the substring and the nodes have matched so far,
			// we have found the str substring in text and can return 1
			if (!str_node)
				return 1;
		}

		text_node = text_node->next;
	}

	// no matches were found, return 0
	return 0;
}

// deallocate list_string memory
void list_string_destroy(struct list_string *ls) {
	assert(ls);

	// deallocate the memory used by each node in ls
	struct ls_node *cur_node = ls->head, *next_node;
	while (cur_node) {
		next_node = cur_node->next;
		free(cur_node);
		cur_node = next_node;
	}

	// deallocate ls itself
	free(ls);
}

