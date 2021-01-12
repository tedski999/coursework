//   list_string_main.c
//   David Gregg
//   January 2021

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "list_string.h"

int main() {
  struct list_string * s1 = new_list_string("abc");
  struct list_string * s2 = new_list_string("abcd");
  struct list_string * s3 = new_list_string("Abcd");
  int nerrors = 0;

  // check the length function is working
  int length = list_string_length(s3);
  if ( length != 4 ) {
    fprintf(stderr, "Error: length of Abcd should be 4\n");
    nerrors++;
  }

  // check the string compare function is working
  if ( list_string_compare(s1, s2 ) >= 0) {
    fprintf(stderr, "Error: abc should be less than abcd\n");
    nerrors++;
  }
  if ( list_string_compare(s3, s2 ) >= 0) {
    fprintf(stderr, "Error: Abcd should be less than abcd\n");
    nerrors++;
  }
  if ( list_string_compare(s2, s2 ) != 0) {
    fprintf(stderr, "Error: abcd should be equal to abcd\n");
    nerrors++;
  }
  
  // check the substring function is working
  int is_substring = list_string_substring(s2, s1);
  if ( is_substring != 1 ) {
    fprintf(stderr, "Error: abc should be a substring of abcd\n");
    nerrors++;
  }

    fprintf(stderr, "%d errors encountered\n", nerrors);

  return 0;
}
