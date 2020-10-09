// Program that reads in a text file of integers
// and outputs the numbers in sorted order.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// find smallest element between start and end of array
int find_min(int a[], int size, int start) {
	int min = a[start];
	int result = start;
	for (int i = start; i < size; i++) {
		if (a[i] < min) {
			min = a[i];
			result = i;
		}
	}
	return result;
}

// selection sort algorithm: repeatedly find smallest
// element and place at start of unsorted section.
int selectionSort(int a[], int size) {
	int itr_count = 0;
	for (int i = 0; i < size - 1; i++) {
		int min = find_min(a, size, i);
		int temp = a[i];
		a[i] = a[min];
		a[min] = temp;
		itr_count++;
	}
	return itr_count;
}

// insertion sort algorithm: place unsorted array
// elements into the correct location in an initially
// empty sorted part.
int insertionSort(int a[], int size) {
	int itr_count = 0;
	for (int i = 1; i < size; i++) {
		int j = i, insert_value = a[i];
		while (--j >= 0 && a[j] > insert_value) {
			a[j + 1] = a[j];
			itr_count++;
		}
		a[j + 1] = insert_value;
	}
	return itr_count;
}

// bubble sort algorithm: repeatedly compare and swap
// adjacent array elements.
int bubbleSort(int a[], int size) {
	int itr_count = 0;
	for (int i = 0; i < size - 1; i++) {
		for (int j = 0; j < size - i - 1; j++) {
			if (a[j] > a[j + 1]) {
				int temp = a[j];
				a[j] = a[j + 1];
				a[j + 1] = temp;
			}
			itr_count++;
		}
	}
	return itr_count;
}

// funny sort algorithm: like bubble sort only we
// go back to the start every time we find a pair
// out of order.
int funnySort(int a[], int size) {
	int temp, j = 0, itr_count = 0;
	while (j < size - 1) {
		if (a[j] > a[j + 1]) {
			temp = a[j];
			a[j] = a[j + 1];
			a[j + 1] = temp;
			j = 0;
		} else {
			j++;
		}
		itr_count++;
	}
	return itr_count;
}

// Open a file. Abort progam if file cannot be opened
FILE *open_file(char filename[]) {
	FILE *file = fopen(filename, "r");
	if (file == NULL) {
		printf("FATAL: Error opening file %s. Aborting program.\n", filename);
		exit(1);
	}

	return file;
}

// read a stream of up to 'size' integers from a text file.
// return number of integers
int read_in(int a[], int size, char filename[]) {
	const int max_line = 1024;
	char line[max_line];

	// read in the ints - one per line
	int i = 0;
	FILE *file = open_file(filename);
	char *eof = fgets(line, max_line, file);
	while (eof != NULL && i < size) { // eof == NULL => end of file
		sscanf(line, "%d", &a[i++]);
		eof = fgets(line, max_line, file);
	}
	fclose(file);
	return i;
}

// write out an array of integers up to 'size'
void write_out(int a[], int size) {
	for (int i = 0; i < size; i++)
		printf("%d\n", a[i]);
}

// copy values from one int array to another
void copy_array(int src[], int dst[], int size) {
	for (int i = 0; i < size; i++)
		dst[i] = src[i];
}

// read in a file of numbers, sort them, and
// write them out to the screen
int main() {
	const int size = 1024;
	int nums[size], temp_nums[size];
	int nnums, iterations_required;

	nnums = read_in(nums, size, "numbers.txt");

	copy_array(nums, temp_nums, size);
	iterations_required = funnySort(temp_nums, nnums);
	printf("\nFunnySort\nIterations required: %i\nSorting results:\n", iterations_required);
	write_out(temp_nums, nnums);

	copy_array(nums, temp_nums, size);
	iterations_required = bubbleSort(temp_nums, nnums);
	printf("\nBubbleSort\nIterations required: %i\nSorting results:\n", iterations_required);
	write_out(temp_nums, nnums);

	copy_array(nums, temp_nums, size);
	iterations_required = insertionSort(temp_nums, nnums);
	printf("\nInsertionSort\nIterations required: %i\nSorting results:\n", iterations_required);
	write_out(temp_nums, nnums);

	copy_array(nums, temp_nums, size);
	iterations_required = selectionSort(temp_nums, nnums);
	printf("\nSelectionSort\nIterations required: %i\nSorting results:\n", iterations_required);
	write_out(temp_nums, nnums);

	return 0;
}
