#include "player.h"
#include <cb.h>
#include <cglm/types.h>

struct data {
	vec3 velocity;
};

static void on_update(struct cb_entity *player, struct cb *cb) {
	struct data *data = player->data;
	struct cb_input *i = &cb->input;
	struct cb_transform *c = &cb->camera.transform;

	cb->camera.fov = glm_clamp(cb->camera.fov * (1 + i->scroll[vY] * 0.1), 0, M_PI_2);

	double sensitivity = cb->camera.fov * 0.0025;
	c->rotation[vX] += sensitivity * 10 * (i->keys[CB_KEY_UP] & CB_IS_KEY_DOWN);
	c->rotation[vX] -= sensitivity * 10 * (i->keys[CB_KEY_DOWN] & CB_IS_KEY_DOWN);
	c->rotation[vX] = glm_clamp(c->rotation[vX] - i->dmouse[vY] * sensitivity, -M_PI_2, M_PI_2);
	c->rotation[vY] -= sensitivity * 10 * (i->keys[CB_KEY_LEFT] & CB_IS_KEY_DOWN);
	c->rotation[vY] += sensitivity * 10 * (i->keys[CB_KEY_RIGHT] & CB_IS_KEY_DOWN);
	c->rotation[vY] += i->dmouse[vX] * sensitivity;

	vec3 forward = { sin(c->rotation[vY]) * cos(c->rotation[vX]), sin(c->rotation[vX]), cos(c->rotation[vY]) * -cos(c->rotation[vX])};
	vec3 up =      { 0, 1, 0};
	vec3 right =   { cos(-c->rotation[vY]), 0, -sin(-c->rotation[vY])};

	vec3 direction = GLM_VEC3_ZERO_INIT;
	if (i->keys[CB_KEY_W] & CB_IS_KEY_DOWN) glm_vec3_add(direction, forward, direction);
	if (i->keys[CB_KEY_S] & CB_IS_KEY_DOWN) glm_vec3_sub(direction, forward, direction);
	if (i->keys[CB_KEY_D] & CB_IS_KEY_DOWN) glm_vec3_add(direction, right, direction);
	if (i->keys[CB_KEY_A] & CB_IS_KEY_DOWN) glm_vec3_sub(direction, right, direction);
	if (i->keys[CB_KEY_SPACE] & CB_IS_KEY_DOWN) glm_vec3_add(direction, up, direction);
	if (i->keys[CB_KEY_SHIFT] & CB_IS_KEY_DOWN) glm_vec3_sub(direction, up, direction);
	if (i->keys[CB_KEY_CONTROL] & CB_IS_KEY_DOWN) {
		glm_vec3_scale(data->velocity, 0.9, data->velocity);
		if (fabs(data->velocity[vX]) + fabs(data->velocity[vY]) + fabs(data->velocity[vZ])  < 0.05)
			glm_vec3_zero(data->velocity);

	}
	glm_normalize(direction);

	double speed = 0.02;
	vec3 acceleration = GLM_VEC3_ZERO_INIT;
	glm_vec3_scale(direction, speed, acceleration);
	glm_vec3_add(data->velocity, acceleration, data->velocity);

	glm_vec3_add(c->position, data->velocity, c->position);

	if (i->keys[CB_KEY_ESCAPE] == CB_ON_KEY_DOWN) {
		cb->is_running = false;
	}
}

static void on_stop(struct cb_entity *player) {
	free(player->data);
}

struct cb_entity *ww_player(struct cb_entity *parent) {
	struct cb_entity *player = cb_entity(parent);
	player->update_fn = on_update;
	player->stop_fn = on_stop;

	struct data *data = malloc(sizeof *data);
	glm_vec3((vec4){0,0,0}, data->velocity);
	player->data = data;

	return player;
}
