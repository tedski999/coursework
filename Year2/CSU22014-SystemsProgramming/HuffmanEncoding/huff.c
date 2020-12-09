#include <stdio.h>
#include <stdlib.h>
#include "huff.h"

#define true  1
#define false 0

void huffcoder_internal_generateCodes(struct huffcoder *coder, struct huffchar *node, int *path, int depth);
void huffcoder_internal_destroy_huffchar(struct huffchar *node);

// create a new huffcoder structure
struct huffcoder *huffcoder_new() {
	struct huffcoder *new_coder = calloc(1, sizeof *new_coder);
	if (!new_coder) {
		fprintf(stderr, "memalloc failure\n");
		exit(1);
	}
	return new_coder;
}

// count the frequency of characters in a file; set chars with zero frequency to one
void huffcoder_count(struct huffcoder *coder, char *filename) {
	if (!coder)
		return;

	// load filename
	FILE *file = fopen(filename, "r");
	if (!file) {
		fprintf(stderr, "Unable to open file '%s' for reading!\n", filename);
		huffcoder_destroy(coder);
		exit(1);
	}

	// reset coder->freqs to zero (should be done with memset really, but thats from <string.h>)
	for (int i = 0; i < NUM_CHARS; i++)
		coder->freqs[i] = 0;

	// read entire file character-by-character
	while (!feof(file))
		coder->freqs[fgetc(file)]++;
	fclose(file);

	// replace frequencies 0 with 1
	for (int i = 0; i < NUM_CHARS; i++)
		if (!coder->freqs[i])
			coder->freqs[i] = 1;
}

// using the character frequencies, build the tree of compound
// and simple characters that are used to compute the Huffman codes
void huffcoder_build_tree(struct huffcoder *coder) {
	if (!coder)
		return;

	// in a perfect world, this would be a priority queue ordered by the freqs field
	int node_list_count = NUM_CHARS;
	struct huffchar **node_list = malloc(sizeof (struct huffchar *) * node_list_count);
	if (!node_list) {
		fprintf(stderr, "memalloc failure\n");
		huffcoder_destroy(coder);
		return;
	}

	int seqno = 0;

	// populate list with pointers to leaf nodes
	for (int i = 0; i < node_list_count; i++) {
		node_list[i] = malloc(sizeof **node_list);
		if (!node_list[i]) {
			fprintf(stderr, "memalloc failure\n");
			huffcoder_destroy(coder);
			exit(1);
		}
		*node_list[i] = (struct huffchar) { coder->freqs[i], false, seqno++, .u.c = i };
	}

	// flesh out the tree with compound nodes
	while (node_list_count > 1) {

		// find the two least frequent nodes still in the list
		// NOTE: sf variables mean smallest frequency
		int sfi1 = 0, sfi2 = 0; // e.g. sfi1 means Smallest Frequency Index 1
		for (int i = 0; i < node_list_count; i++) {
			int freq = node_list[i]->freq, seqno = node_list[i]->seqno;
			int sf1 = node_list[sfi1]->freq, sf1_seqno = node_list[sfi1]->seqno;
			int sf2 = node_list[sfi2]->freq, sf2_seqno = node_list[sfi2]->seqno;
			if (freq < sf1 || (freq == sf1 && seqno < sf1_seqno)) {
				sfi2 = sfi1;
				sfi1 = i;
			} else if (sfi1 == sfi2 || freq < sf2 || (freq == sf2 && seqno < sf2_seqno)) {
				sfi2 = i;
			}
		}

		// create a new compound node from the two nodes
		struct huffchar *huffchar1 = node_list[sfi1];
		struct huffchar *huffchar2 = node_list[sfi2];
		struct huffchar *new_huffchar = malloc(sizeof *new_huffchar);
		*new_huffchar = (struct huffchar) {
			huffchar1->freq + huffchar2->freq, true, seqno++,
			.u.compound = { huffchar1, huffchar2 }
		};

		// replace used nodes with the compound and last list element,
		// effectively shrinking the list by one.
		node_list[sfi1] = new_huffchar;
		node_list[sfi2] = node_list[--node_list_count];
	}

	// after the tree is generated, there is only one node left - the root
	coder->tree = node_list[0];
	free(node_list);
}

// using the Huffman tree, build a table of the Huffman codes with the huffcoder object
void huffcoder_tree2table(struct huffcoder *coder) {
	huffcoder_internal_generateCodes(coder, coder->tree, NULL, 0);
}

// print the Huffman codes for each character in order
void huffcoder_print_codes(struct huffcoder *coder) {
	for (int i = 0; i < NUM_CHARS; i++)
		printf("char: %d, freq: %d, code: %s\n", i, coder->freqs[i], coder->codes[i]);
}

// encode the input file and write the encoding to the output file
void huffcoder_encode(struct huffcoder *coder, char *input_filename, char *output_filename) {

	/*
	// initialize everything
	huffcoder_count(coder, input_filename);
	huffcoder_build_tree(coder);
	huffcoder_tree2table(coder);

	// load files
	FILE *in_file = fopen(input_filename, "r");
	FILE *out_file = fopen(output_filename, "w");
	if (!in_file || !out_file) {
		fprintf(stderr, "Unable to open files '%s' and '%s'!\n", input_filename, output_filename);
		huffcoder_destroy(coder);
		fclose(in_file);
		fclose(out_file);
		exit(1);
	}

	// read entire file character-by-character
	while (!feof(in_file)) {
		char c = fgetc(in_file);
		if (c == EOF)
			break;
		fprintf(out_file, "%s", coder->codes[(int) c]);
	}

	// cleanup
	huffcoder_destroy(coder);
	fclose(in_file);
	fclose(out_file);
	*/
}

// decode the input file and write the decoding to the output file
void huffcoder_decode(struct huffcoder *coder, char *input_filename, char *output_filename) {

}

// delete a huffcoder structure
void huffcoder_destroy(struct huffcoder *coder) {
	if (!coder)
		return;
	for (int i = 0; i < NUM_CHARS; i++)
		free(coder->codes[i]);
	huffcoder_internal_destroy_huffchar(coder->tree);
}

void huffcoder_internal_generateCodes(struct huffcoder *coder, struct huffchar *node, int *path, int depth) {
	if (node->is_compound) {
		// recursively generate codes for subnodes
		int *new_path = malloc(sizeof *new_path * (depth + 1));
		for (int i = 0; i < depth; i++)
			new_path[i] = path[i];
		new_path[depth] = 0;
		huffcoder_internal_generateCodes(coder, node->u.compound.left, new_path, depth + 1);
		new_path[depth] = 1;
		huffcoder_internal_generateCodes(coder, node->u.compound.right, new_path, depth + 1);
		free(new_path);
	} else {
		// convert current path to a code
		char *code = calloc(depth, 1);
		for (int i = 0; i < depth; i++)
			code[i] = path[i] ? '1' : '0';
		coder->codes[node->u.c] = code;
		coder->code_lengths[node->u.c] = depth;
	}
}

// delete a huffchar structure and recursively any subnodes
void huffcoder_internal_destroy_huffchar(struct huffchar *node) {
	if (node && node->is_compound) {
		huffcoder_internal_destroy_huffchar(node->u.compound.left);
		huffcoder_internal_destroy_huffchar(node->u.compound.right);
	}
	free(node);
}

