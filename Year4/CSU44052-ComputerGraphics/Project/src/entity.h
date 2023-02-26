#ifndef WW_ENTITY_H
#define WW_ENTITY_H

#include <cglm/cglm.h>

enum ww_vector_index { vX, vY, vZ, vW };
struct ww_transform { vec3 position, rotation, scale; };
struct ww_transforms { mat4 m, v, p, mv, mvp; };

struct ww_entity {
	struct ww_transform transform, transform_old;
	struct ww_entity **children; int children_len;
	void (*update_fn)(struct ww_entity *entity);
	void (*render_fn)(struct ww_entity *entity, struct ww_transforms *transforms, double interp);
	void (*cleanup_fn)(struct ww_entity *entity);
	void *data;
};

struct ww_entity *ww_entity_create(void);
void ww_entity_attach(struct ww_entity *parent, struct ww_entity *entity);
void ww_entity_detach(struct ww_entity *parent, struct ww_entity *entity);
void ww_entity_preupdate(struct ww_entity *entity);
void ww_entity_update(struct ww_entity *entity);
void ww_entity_render(struct ww_entity *entity, struct ww_transforms transforms, double interp);
void ww_entity_destroy(struct ww_entity *entity);

#endif
