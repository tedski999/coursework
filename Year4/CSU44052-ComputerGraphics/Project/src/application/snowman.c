#include "snowman.h"
#include "ground.h"
#include "present.h"
#include "../alloc.h"
#include "../entity.h"
#include "../model.h"
#include <string.h>

extern struct ww_entity *world;

enum state { IDLE, WALKING, CARRYING };

struct data {
	double time;
	struct ww_entity *ground;
	struct ww_entity **presents;
	int presents_len;
	struct ww_model *model;
	GLuint shader;
	GLuint u_transform;
	GLuint u_bones;
	enum state state;
	int animation;
	vec2 target_position;
	struct ww_entity *target_present;
};

static void apply_skeletal_animation(struct ww_model_animation *animation, struct ww_model *model, struct ww_model_node *node, mat4 parent_transform, double time) {
	mat4 *key_transform = NULL;
	for (int c = 0; c < animation->channels_len && !key_transform; c++)
		if (!strcmp(animation->channels[c].name, node->name))
			for (int k = 0; k < animation->channels[c].keys_len && !key_transform; k++)
				if (time < animation->channels[c].keys[k].time)
					key_transform = &animation->channels[c].keys[k].transform;

	mat4 transform;
	glm_mat4_mul(parent_transform, key_transform ? *key_transform : node->transform, transform);

	int bone_index = 0;
	for (; bone_index < model->bones_len; bone_index++)
		if (!strcmp(node->name, model->bones[bone_index].name))
			break;
	if (bone_index != model->bones_len)
		glm_mat4_mul(transform, model->bones[bone_index].offset, model->bones[bone_index].current);

	for (int i = 0; i < node->children_len; i++)
		apply_skeletal_animation(animation, model, &node->children[i], transform, time);
}

static void on_update(struct ww_entity *snowman) {
	struct data *data = snowman->data;
	struct ww_model_animation *animation = &data->model->animations[data->animation];
	data->time = fmod(data->time + animation->tps, animation->duration);

	double speed = 0;

	switch (data->state) {
		case IDLE:
			if (rand()%200==0) {
				data->target_position[0] = (double)rand()/RAND_MAX*50-100;
				data->target_position[1] = (double)rand()/RAND_MAX*50-100;
			}
			if (rand()%500==0) {
				struct ww_entity *new_target = data->presents[rand()%data->presents_len];
				struct ww_present_data *target_data = new_target->data;
				if (target_data->target) break;
				target_data->target = snowman;
				data->target_present = new_target;
				glm_vec2_copy((vec2){new_target->transform.position[vX],new_target->transform.position[vZ]}, data->target_position);
				data->animation = 2;
				data->state = WALKING;
			}
			break;
		case WALKING: {
			speed = 0.05;
			vec2 s = {snowman->transform.position[vX],snowman->transform.position[vZ]};
			if (glm_vec2_distance(s, data->target_position) > 2)
				break;
			data->state = CARRYING;
			data->target_position[0] = (double)rand()/RAND_MAX*100-50;
			data->target_position[1] = (double)rand()/RAND_MAX*100-50;
			data->animation = 0;
			ww_entity_detach(world, data->target_present);
			ww_entity_attach(snowman, data->target_present);
			glm_vec3_copy((vec3){0,2.5,-1.25}, data->target_present->transform.position);
			glm_vec3_copy((vec3){0,0,0}, data->target_present->transform.rotation);
			glm_vec3_copy((vec3){0.5,0.5,0.5}, data->target_present->transform.scale);
			glm_vec3_copy(data->target_present->transform.position, data->target_present->transform_old.position);
			break;
		}
		case CARRYING:
			speed = 0.03;
			vec2 s = {snowman->transform.position[vX],snowman->transform.position[vZ]};
			if (glm_vec2_distance(s, data->target_position) > 2)
				break;
			data->state = IDLE;
			data->animation = 1;
			struct ww_present_data *present_data = data->target_present->data;
			present_data->target = NULL;
			ww_entity_detach(snowman, data->target_present);
			ww_entity_attach(world, data->target_present);
			glm_vec3_copy(snowman->transform.position, data->target_present->transform.position);
			glm_vec3_copy(snowman->transform.rotation, data->target_present->transform.rotation);
			glm_vec3_copy((vec3){1,1,1}, data->target_present->transform.scale);
			data->target_present->transform.position[vX] -= sin(data->target_present->transform.rotation[vY]) * 3;
			data->target_present->transform.position[vZ] -= cos(data->target_present->transform.rotation[vY]) * 3;
			data->target_present->transform.position[vY] = ww_ground_get_height(data->ground, data->target_present->transform.position[vZ], data->target_present->transform.position[vX]);
			glm_vec3_copy(data->target_present->transform.position, data->target_present->transform_old.position);
			data->target_present = NULL;
			break;
	}

	double target_angle = atan2(
		snowman->transform.position[vX] - data->target_position[0],
		snowman->transform.position[vZ] - data->target_position[1]);
	snowman->transform.rotation[vY] = glm_lerp(snowman->transform.rotation[vY], fmod(target_angle, M_PI*2), 0.025);
	snowman->transform.position[vX] -= sin(snowman->transform.rotation[vY]) * speed;
	snowman->transform.position[vZ] -= cos(snowman->transform.rotation[vY]) * speed;
	snowman->transform.position[vY] = ww_ground_get_height(data->ground, snowman->transform.position[vZ], snowman->transform.position[vX]) - 2;
	ww_ground_deform(data->ground, snowman->transform.position[vZ], snowman->transform.position[vX], 2.0);
}

static void on_render(struct ww_entity *snowman, struct ww_transforms *transforms, double interp) {
	struct data *data = snowman->data;

	glUseProgram(data->shader);

	struct ww_model_animation *animation = &data->model->animations[data->animation];
	apply_skeletal_animation(animation, data->model, &data->model->root_node, GLM_MAT4_IDENTITY, data->time);
	for (int i = 0; i < data->model->bones_len; i++) {
		char name[snprintf(NULL, 0, "u_bones[%d]", i) + 1];
		snprintf(name, sizeof name, "u_bones[%d]", i);
		GLuint u_bone = glGetUniformLocation(data->shader, name);
		glUniformMatrix4fv(u_bone, 1, GL_FALSE, *data->model->bones[i].current);
	}

	glUniformMatrix4fv(data->u_transform, 1, GL_FALSE, *transforms->mvp);
	glUniformMatrix4fv(glGetUniformLocation(data->shader, "u_model"), 1, GL_FALSE, *transforms->m);

	ww_model_render(data->model);
}

static void on_destroy(struct ww_entity *snowman) {
	ww_free(snowman->data);
}

struct ww_entity *ww_snowman(struct ww_model *model, GLuint shader, struct ww_entity *ground, struct ww_entity **presents, int presents_len) {
	struct ww_entity *snowman = ww_entity_create();
	struct data *data = ww_malloc(sizeof *data);

	snowman->update_fn = on_update;
	snowman->render_fn = on_render;
	snowman->cleanup_fn = on_destroy;

	snowman->data = data;
	*data = (struct data) {
		0, ground, presents, presents_len, model, shader,
		glGetUniformLocation(shader, "u_transform"),
		glGetUniformLocation(shader, "u_bones"),
		IDLE, 1, {0,0}
	};

	data->target_position[0] = (double)rand()/RAND_MAX*50-100;
	data->target_position[1] = (double)rand()/RAND_MAX*50-100;

	for (int m = 0; m < model->meshes_len; m++) {
		struct ww_model_mesh *mesh = &model->meshes[m];
		int vertices_len = mesh->vertices_len;
		struct vertex { vec3 position; vec3 normal; vec4 bone_weights; ivec4 bone_indices; };
		struct vertex vertices[mesh->vertices_len];
		for (int v = 0; v < vertices_len; v++) {
			struct ww_model_mesh_vertex *vertex = &mesh->vertices[v];
			glm_vec3_copy(vertex->position, vertices[v].position);
			glm_vec3_copy(vertex->normal, vertices[v].normal);
			glm_vec4_copy(vertex->bone_weights, vertices[v].bone_weights);
			glm_ivec4_copy(vertex->bone_indices, vertices[v].bone_indices);
		}
		glBindVertexArray(mesh->vao);
		glBindBuffer(GL_ARRAY_BUFFER, mesh->vbo);
		glBufferData(GL_ARRAY_BUFFER, vertices_len * sizeof *vertices, vertices, GL_STATIC_DRAW);

		enum { POSITION, NORMAL, BONE_WEIGHTS, BONE_INDICES };
		glEnableVertexAttribArray(POSITION);
		glEnableVertexAttribArray(NORMAL);
		glEnableVertexAttribArray(BONE_WEIGHTS);
		glEnableVertexAttribArray(BONE_INDICES);
		glVertexAttribPointer (POSITION,     3, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, position)));
		glVertexAttribPointer (NORMAL,       3, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, normal)));
		glVertexAttribPointer (BONE_WEIGHTS, 4, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, bone_weights)));
		glVertexAttribIPointer(BONE_INDICES, 4, GL_INT,             sizeof *vertices, (void *) (offsetof(struct vertex, bone_indices)));

		glBindVertexArray(0);
	}

	return snowman;
}
