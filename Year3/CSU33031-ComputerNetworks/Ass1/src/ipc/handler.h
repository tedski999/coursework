#ifndef SUBPUB_IPC_HANDLER_H
#define SUBPUB_IPC_HANDLER_H

#include "../deadline/list.h"
#include <stdbool.h>

bool subpub_ipc_handle(int fd, int sockfd, struct subpub_deadline_list *deadline_list);

#endif
