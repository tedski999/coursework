#ifndef SUBPUB_IPC_FIFO_H
#define SUBPUB_IPC_FIFO_H

int subpub_ipc_fifo_create(char *path);
char *subpub_ipc_fifo_read(int fd);
int subpub_ipc_fifo_clear(int fd, char *path);
void subpub_ipc_fifo_destroy(int fd, char *path);

#endif
