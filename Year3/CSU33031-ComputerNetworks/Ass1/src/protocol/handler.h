#ifndef SUBPUB_PROTOCOL_HANDLER_H
#define SUBPUB_PROTOCOL_HANDLER_H

#include "../net/address.h"
#include "../deadline/list.h"

void subpub_protocol_handle_incoming_data(int sockfd, struct subpub_deadline_list *deadline_list);
void subpub_protocol_handle_partial_timeout(int sockfd, struct subpub_net_address address, char header, char index);

#endif
