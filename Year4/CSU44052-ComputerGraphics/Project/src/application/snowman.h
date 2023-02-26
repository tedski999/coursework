#ifndef WW_APPLICATION_SNOWMAN_H
#define WW_APPLICATION_SNOWMAN_H

#include "../model.h"

struct ww_entity *ww_snowman(struct ww_model *model, GLuint shader, struct ww_entity *ground, struct ww_entity **presents, int presents_len);

#endif
