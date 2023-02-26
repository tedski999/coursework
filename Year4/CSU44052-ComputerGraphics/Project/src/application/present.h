#ifndef WW_APPLICATION_PRESENT_H
#define WW_APPLICATION_PRESENT_H

#include "../model.h"

struct ww_present_data {
	struct ww_entity *target;
};

struct ww_entity *ww_present(struct ww_model *model, GLuint shader, GLuint texture);

#endif
