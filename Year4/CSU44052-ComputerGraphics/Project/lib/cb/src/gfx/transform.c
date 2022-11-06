#include "transform.h"
#include <cb.h>
#include <cglm/cglm.h>

void cbi_transform_update(struct cb_transform *transform) {
	glm_vec3_copy(transform->position, transform->last_position);
	glm_vec3_copy(transform->rotation, transform->last_rotation);
	glm_vec3_copy(transform->scale, transform->last_scale);
}

void cbi_transform_lerp(struct cb_transform *transform, double interpolation, struct cb_transform *lerp) {
	glm_vec3_lerpc(transform->last_position, transform->position, interpolation, lerp->position);
	glm_vec3_lerpc(transform->last_rotation, transform->rotation, interpolation, lerp->rotation);
	glm_vec3_lerpc(transform->last_scale, transform->scale, interpolation, lerp->scale);
}

void cbi_transform_matrix(struct cb_transform *transform, mat4 *matrix) {
	glm_mat4_identity(*matrix);
	glm_translate(*matrix, transform->position);
	glm_rotate_z(*matrix, transform->rotation[vZ], *matrix);
	glm_rotate_y(*matrix, transform->rotation[vY], *matrix);
	glm_rotate_x(*matrix, transform->rotation[vX], *matrix);
	glm_scale(*matrix, transform->scale);
}

void cbi_transform_inverse_matrix(struct cb_transform *transform, mat4 *matrix) {
	glm_mat4_identity(*matrix);
	glm_scale(*matrix, transform->scale);
	glm_rotate_x(*matrix, transform->rotation[vX], *matrix);
	glm_rotate_y(*matrix, transform->rotation[vY], *matrix);
	glm_rotate_z(*matrix, transform->rotation[vZ], *matrix);
	glm_translate(*matrix, transform->position);
}
