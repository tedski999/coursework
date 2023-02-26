#include "entity.h"
#include "assert.h"
#include "config.h"
#include "model.h"
#include "alloc.h"
#include "log.h"

struct ww_entity *ww_entity_create(void) {
	struct ww_entity *entity = ww_calloc(1, sizeof *entity);
	glm_vec3_one(entity->transform.scale);
	return entity;
}

void ww_entity_attach(struct ww_entity *parent, struct ww_entity *entity) {
	parent->children = ww_realloc(parent->children, sizeof (struct ww_entity *) * (parent->children_len += 1));
	parent->children[parent->children_len - 1] = entity;
}

void ww_entity_detach(struct ww_entity *parent, struct ww_entity *entity) {
	for (int i = 0; i < parent->children_len; i++)
		if (parent->children[i] == entity)
			parent->children[i] = parent->children[(parent->children_len -= 1)];
}

void ww_entity_preupdate(struct ww_entity *entity) {
	glm_vec3_copy(entity->transform.position, entity->transform_old.position);
	glm_vec3_copy(entity->transform.rotation, entity->transform_old.rotation);
	glm_vec3_copy(entity->transform.scale, entity->transform_old.scale);
	for (int i = 0; i < entity->children_len; i++)
		ww_entity_preupdate(entity->children[i]);
}

void ww_entity_update(struct ww_entity *entity) {
	if (entity->update_fn)
		entity->update_fn(entity);
	for (int i = 0; i < entity->children_len; i++)
		ww_entity_update(entity->children[i]);
}

void ww_entity_render(struct ww_entity *entity, struct ww_transforms transforms, double interp) {
	struct ww_transform lerp;
	glm_vec3_lerpc(entity->transform_old.position, entity->transform.position, interp, lerp.position);
	glm_vec3_lerpc(entity->transform_old.rotation, entity->transform.rotation, interp, lerp.rotation);
	glm_vec3_lerpc(entity->transform_old.scale, entity->transform.scale, interp, lerp.scale);

	glm_translate_make(transforms.m, lerp.position);
	glm_rotate_z(transforms.m, lerp.rotation[vZ], transforms.m);
	glm_rotate_y(transforms.m, lerp.rotation[vY], transforms.m);
	glm_rotate_x(transforms.m, lerp.rotation[vX], transforms.m);
	glm_scale(transforms.m, lerp.scale);
	glm_mat4_mul(transforms.v, transforms.m, transforms.mv);
	glm_mat4_mul(transforms.p, transforms.mv, transforms.mvp);

	if (entity->render_fn)
		entity->render_fn(entity, &transforms, interp);

	glm_mat4_copy(transforms.mv, transforms.v);
	for (int i = 0; i < entity->children_len; i++)
		ww_entity_render(entity->children[i], transforms, interp);
}

void ww_entity_destroy(struct ww_entity *entity) {
	if (entity->cleanup_fn)
		entity->cleanup_fn(entity);
	ww_free(entity->children);
	ww_free(entity);
}
