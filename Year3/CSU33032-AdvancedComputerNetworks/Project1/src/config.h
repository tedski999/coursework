#ifndef WP_CONFIG_H
#define WP_CONFIG_H

#define LISTENING_PORT "4000"
#define TUNNEL_METHOD "CONNECT"
#define HTTP_VIA_HEADER "Via: 1.1 WebProxy"
#define MSG_BUFFER_SIZE (1024*32)
#define RECV_TIMEOUT (3)
#undef PRINT_HTTP_MSGS

#endif
