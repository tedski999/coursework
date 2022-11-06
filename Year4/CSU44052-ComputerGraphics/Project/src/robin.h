#ifndef WW_ROBIN_H
#define WW_ROBIN_H

#include <cb.h>

struct cb_entity *ww_robin(
		struct cb_entity *parent,
		struct cb_model *body_model, struct cb_texture *body_texture,
		struct cb_model *left_leg_model, struct cb_texture *left_leg_texture,
		struct cb_model *right_leg_model, struct cb_texture *right_leg_texture,
		struct cb_model *left_wing_model, struct cb_texture *left_wing_texture,
		struct cb_model *right_wing_model, struct cb_texture *right_wing_texture,
		struct cb_model *tail_model, struct cb_texture *tail_texture,
		struct cb_shader *shader);

#endif
