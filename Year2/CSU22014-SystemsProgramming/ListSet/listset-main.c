#include "listset.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

static const char *usage =
	"Usage: ./listset <command> [<string>] ...\n"
	"\twhere <command> is\n"
	"\t\t+ (add following string to active set)\n"
	"\t\t- (remove following string from active set)\n"
	"\t\t~ (swap currently active set)\n"
	"\t\tc (copy active set into alternative set)\n"
	"\t\tu (union both sets into active set)\n"
	"\t\tn (intersect both sets into active set)\n"
	"\t\t= (print active set)\n";

int main(int argc, char **argv) {
	if (argc <= 1) {
		fprintf(stderr, "%s", usage);
		exit(1);
	}

	int active_set = 0;
	struct listset *sets[2] = { listset_new(), listset_new() };

	int i = 1;
	while (i < argc) {
		char *arg = argv[i++];

		if (strlen(arg) != 1) // a command must only be one character
			arg = "";

		switch(arg[0]) {
			case '~': active_set = !active_set;                              break;
			case 'c': listset_copy(sets[!active_set], sets[active_set]);     break;
			case 'u': listset_union(sets[active_set], sets[0], sets[1]);     break;
			case 'n': listset_intersect(sets[active_set], sets[0], sets[1]); break;
			case '=': listset_print(sets[active_set]);                       break;

			case '+':
				if (i >= argc) {
					fprintf(stderr, "Error: No string provided for '+' command\n");
					exit(1);
				}
				listset_add(sets[active_set], argv[i++]);
				break;

			case '-':
				if (i >= argc) {
					fprintf(stderr, "Error: No string provided for '-' command\n");
					exit(1);
				}
				listset_remove(sets[active_set], argv[i++]);
				break;

			default:
				fprintf(stderr, "Error: Unknown command '%s'\n", argv[--i]);
				exit(1);
		}
	}

	listset_destory(sets[0]);
	listset_destory(sets[1]);

	return 0;
}

