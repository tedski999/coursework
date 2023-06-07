#ifndef CB_ENTITY_H
#define CB_ENTITY_H

#include <cglm/cglm.h>

struct cb;

enum { vX, vY, vZ, vW };

struct cb_transform { vec3 position, rotation, scale; };

struct cb_entity {
	void (*update_fn)(struct cb_entity *entity, struct cb *cb);
	void (*render_fn)(struct cb_entity *entity, mat4 model_mat, mat4 modelview_mat, double interpolation);
	struct cb_transform transform, old_transform;
	struct cb_entity *parent, **children;
	int children_len;
	int instance_id;
};

void cb_entity_preupdate(struct cb_entity *entity);
void cb_entity_update(struct cb_entity *entity, struct cb *cb);
void cb_entity_render(struct cb_entity *entity, mat4 view_mat, double interpolation);
void cb_entity_add_child(struct cb_entity *entity, struct cb_entity *child);
void cb_entity_remove_child(struct cb_entity *entity, struct cb_entity *child);
void cb_entity_cleanup(struct cb_entity *entity);

#endif
