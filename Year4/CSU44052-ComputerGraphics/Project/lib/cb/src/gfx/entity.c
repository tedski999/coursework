#include "entity.h"
#include "transform.h"
#include "../cbi.h"
#include "../data/model.h"
#include "../data/texture.h"
#include "../data/shader.h"
#include "../util/assert.h"
#include "../util/alloc.h"
#include "../util/prof.h"
#include "../util/log.h"
#include <cb.h>
#include <glad/gl.h>
#include <cglm/cglm.h>

static int id;

static struct cb_entity *new_child(struct cb_entity *parent) {
	struct cb_entity *child = cbi_malloc(sizeof *child);
	parent->children_len += 1;
	parent->children = cbi_realloc(parent->children, sizeof (struct cb_entity *) * parent->children_len);
	parent->children[parent->children_len - 1] = child;
	return child;
}

static void remove_child(struct cb_entity *parent, struct cb_entity *child) {
	int child_index = 0;
	for (; child_index < parent->children_len; child_index++)
		if (parent->children[child_index] == child)
			break;
	cbi_assert(parent->children_len != child_index, "Cannot find E%d in children of E%d", child->id, parent->id);
	parent->children_len -= 1;
	parent->children[child_index] = parent->children[parent->children_len];
}

struct cb_entity *cbi_entity(struct cb_entity *parent) {
	int new_id = ++id;
	cbi_log(CB_LOG_DBUG, "Creating E%d as child of E%d", new_id, parent->id);
	cbi_prof(CBI_PROF_EVENT, "Create Entity %d", new_id);
	struct cb_entity *entity = new_child(parent);
	*entity = (struct cb_entity) { .transform.scale = GLM_VEC3_ONE_INIT, .parent = parent, .id = id };
	return entity;
}

void cbi_entity_preupdate(struct cb_entity *entity) {
	cbi_transform_update(&entity->transform);
	for (int i = 0; i < entity->children_len; i++)
		cbi_entity_preupdate(entity->children[i]);
}

void cbi_entity_update(struct cb_entity *entity, struct cbi *cbi) {
	if (entity->update_fn)
		entity->update_fn(entity, &cbi->cb);
	for (int i = 0; i < entity->children_len; i++)
		cbi_entity_update(entity->children[i], cbi);
}

void cbi_entity_render(struct cb_entity *entity, mat4 *frame, mat4 *projection, double interpolation) {
	mat4 m, mv, mvp;
	struct cb_transform lerp;
	cbi_transform_lerp(&entity->transform, interpolation, &lerp);
	cbi_transform_matrix(&lerp, &m);
	glm_mat4_mul(*frame, m, mv);
	glm_mat4_mul(*projection, mv, mvp);

	if (entity->model && entity->shader) {
		glUseProgram(entity->shader->program);
		if (entity->texture) {
			glActiveTexture(GL_TEXTURE0);
			glBindTexture(GL_TEXTURE_2D, entity->texture->id);
			glUniform1i(entity->shader->u_texture, 0);
		} else {
			glBindTexture(GL_TEXTURE_2D, 0);
		}
		glUniformMatrix4fv(entity->shader->u_model, 1, GL_FALSE, &m[0][0]);
		glUniformMatrix4fv(entity->shader->u_modelview, 1, GL_FALSE, &mv[0][0]);
		glUniformMatrix4fv(entity->shader->u_modelviewproj, 1, GL_FALSE, &mvp[0][0]);
		for (int i = 0; i < entity->model->meshes_len; i++) {
			glBindVertexArray(entity->model->meshes[i].vao);
			glDrawElements(GL_TRIANGLES, entity->model->meshes[i].len, GL_UNSIGNED_INT, 0);
		}
	}

	for (int i = 0; i < entity->children_len; i++)
		cbi_entity_render(entity->children[i], &mv, projection, interpolation);
}

void cbi_entity_child(struct cb_entity *parent, struct cb_entity *child) {
	cbi_prof(CBI_PROF_EVENT, "Transfer Entity %d To %d", child->id, parent->id);
	cbi_log(CB_LOG_DBUG, "Transfering E%d to children of E%d", child->id, parent->id);
	remove_child(parent, child);
	*new_child(parent) = *child;
}

void cbi_entity_remove(struct cb_entity *entity) {
	cbi_prof(CBI_PROF_EVENT, "Remove Entity %d", entity->id);
	cbi_log(CB_LOG_DBUG, "Removing E%d", entity->id);
	if (entity->stop_fn)
		entity->stop_fn(entity);
	for (int i = 0; i < entity->children_len; i++) {
		cbi_entity_remove(entity->children[i]);
		cbi_free(entity->children[i]);
	}
	cbi_free(entity->children);
}

void cbi_entity_delete(struct cb_entity *entity) {
	cbi_prof(CBI_PROF_EVENT, "Delete Entity %d", entity->id);
	cbi_log(CB_LOG_DBUG, "Deleting E%d", entity->id);
	cbi_entity_remove(entity);
	remove_child(entity->parent, entity);
	cbi_free(entity);
}
