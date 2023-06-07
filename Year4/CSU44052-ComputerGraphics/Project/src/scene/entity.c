#include "entity.h"
#include "../platform/alloc.h"
#include <cglm/cglm.h>

void cb_entity_preupdate(struct cb_entity *entity) {
	glm_vec3_copy(entity->transform.position, entity->old_transform.position);
	glm_vec3_copy(entity->transform.rotation, entity->old_transform.rotation);
	glm_vec3_copy(entity->transform.scale, entity->old_transform.scale);
	for (int i = 0; i < entity->children_len; i++)
		cb_entity_preupdate(entity->children[i]);
}

void cb_entity_update(struct cb_entity *entity, struct cb *cb) {
	if (entity->update_fn)
		entity->update_fn(entity, cb);
	for (int i = 0; i < entity->children_len; i++)
		cb_entity_update(entity->children[i], cb);
}

void cb_entity_render(struct cb_entity *entity, mat4 view_mat, double interpolation) {
	// TODO: i dont trust this math
	struct cb_transform lerp;
	glm_vec3_lerpc(entity->old_transform.position, entity->transform.position, interpolation, lerp.position);
	glm_vec3_lerpc(entity->old_transform.rotation, entity->transform.rotation, interpolation, lerp.rotation);
	glm_vec3_lerpc(entity->old_transform.scale, entity->transform.scale, interpolation, lerp.scale);
	mat4 model_mat = GLM_MAT4_IDENTITY_INIT, modelview_mat;
	glm_translate(model_mat, lerp.position);
	glm_rotate_z(model_mat, lerp.rotation[vZ], model_mat);
	glm_rotate_y(model_mat, lerp.rotation[vY], model_mat);
	glm_rotate_x(model_mat, lerp.rotation[vX], model_mat);
	glm_scale(model_mat, lerp.scale);
	glm_mat4_mul(view_mat, model_mat, modelview_mat);
	if (entity->render_fn)
		entity->render_fn(entity, model_mat, modelview_mat, interpolation);
	for (int i = 0; i < entity->children_len; i++)
		cb_entity_render(entity->children[i], modelview_mat, interpolation);
}

void cb_entity_add_child(struct cb_entity *entity, struct cb_entity *child) {
	entity->children = cb_realloc(entity->children, sizeof (struct cb_entity *) * (entity->children_len += 1));
	entity->children[entity->children_len - 1] = child;
}

void cb_entity_remove_child(struct cb_entity *entity, struct cb_entity *child) {
	for (int i = 0; i < entity->children_len; i++)
		if (entity->children[i] == child)
			entity->children[i] = entity->children[(entity->children_len -= 1)];
}

void cb_entity_cleanup(struct cb_entity *entity) {
	for (int i = 0; i < entity->children_len; i++)
		cb_entity_cleanup(entity->children[i]);
	cb_free(entity->children);
}
