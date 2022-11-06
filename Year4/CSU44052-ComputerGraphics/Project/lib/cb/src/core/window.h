#ifndef CBI_CORE_WINDOW_H
#define CBI_CORE_WINDOW_H

struct cbi;

struct cbi_window *cbi_window_open(struct cbi *cbi);
void cbi_window_draw(struct cbi_window *window);
void cbi_window_title(struct cbi_window *window, char *title);
void cbi_window_close(struct cbi_window *window);

#endif
