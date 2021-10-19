#include "fifo.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define INITIAL_BUFFER_LEN 32

int subpub_ipc_fifo_create(char *path) {
	unlink(path);
	mkfifo(path, 0666);
	return open(path, O_RDONLY | O_NDELAY);
}

char *subpub_ipc_fifo_read(int fd) {
	int buffer_len = INITIAL_BUFFER_LEN;
	char *buffer = malloc(sizeof *buffer * buffer_len);

	int total_read_len = 0, read_len;
	while (1) {
		read_len = read(
			fd, buffer + total_read_len,
			buffer_len - total_read_len - 1);
		total_read_len += read_len;

		// Error
		if (read_len < 0) {
			free(buffer);
			return NULL;
		}

		// All data read
		if (read_len == 0) {
			buffer[total_read_len] = '\0';
			return buffer;
		}

		// Expand the buffer if needed
		if (total_read_len >= buffer_len - 1) {
			buffer_len *= 2;
			buffer = realloc(buffer, sizeof *buffer * buffer_len);
		}
	}
}

int subpub_ipc_fifo_clear(int fd, char *path) {
	close(fd);
	return open(path, O_RDONLY | O_NDELAY);
}

void subpub_ipc_fifo_destroy(int fd, char *path) {
	close(fd);
	unlink(path);
}
