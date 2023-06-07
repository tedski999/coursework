#ifndef CB_WINDOW_H
#define CB_WINDOW_H

struct cb;

struct GLFWwindow *cb_window_open(struct cb *cb);
void cb_window_close(struct GLFWwindow *window);

#endif
