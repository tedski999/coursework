#include "robin.h"
#include <cb.h>

#include <assert.h>

struct frame {
	int delay;
	struct cb_transform body;
	struct cb_transform left_leg;
	struct cb_transform right_leg;
	struct cb_transform left_wing;
	struct cb_transform right_wing;
	struct cb_transform tail;
};

struct animation {
	int frames_len;
	struct frame *frames;
};

struct data {
	int animation_frame_time;
	int animation_frame_number;
	struct animation *animation;
	// struct frame *animation_last_frame;
	// struct frame *animation_next_frame;
	struct cb_entity *body;
	struct cb_entity *left_leg;
	struct cb_entity *right_leg;
	struct cb_entity *left_wing;
	struct cb_entity *right_wing;
	struct cb_entity *tail;
};

// swing tail back and forth
// open and close wings slightly
static struct frame idle_animation_frames[] = {
	{
		.delay = 100,
		.body = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.left_leg = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.right_leg = { .position = {0,0,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.left_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.right_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.tail = { .position = {0,0,0}, .rotation = {0,0.15,0}, .scale = {1,1,1} },
	},
	{
		.delay = 100,
		.body = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.left_leg = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.right_leg = { .position = {0,0,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.left_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.right_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		.tail = { .position = {0,0,0}, .rotation = {0,-0.15,0}, .scale = {1,1,1} },
	}
};
static struct animation idle_animation = {
	.frames_len = 2,
	.frames = idle_animation_frames
};

static struct frame dance_animation_frames[] = {
		{
			.delay = 20,
			.body = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_leg = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_leg = { .position = {0,0.1,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.tail = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		},
		{
			.delay = 20,
			.body = { .position = {0,0,0.1}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_leg = { .position = {0,0.1,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_leg = { .position = {0,0,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_wing = { .position = {0.0,0,-0.7}, .rotation = {0,1.5,0}, .scale = {1,1,1} },
			.tail = { .position = {0,0,0}, .rotation = {0,0.3,0}, .scale = {1,1,1} },
		},
		{
			.delay = 20,
			.body = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_leg = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_leg = { .position = {0,0.1,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.tail = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
		},
		{
			.delay = 20,
			.body = { .position = {0,0,-0.1}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_leg = { .position = {0,0.1,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.right_leg = { .position = {0,0,-0.35}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.left_wing = { .position = {0.0,0,0.7}, .rotation = {0,-1.5,0}, .scale = {1,1,1} },
			.right_wing = { .position = {0,0,0}, .rotation = {0,0,0}, .scale = {1,1,1} },
			.tail = { .position = {0,0,0}, .rotation = {0,-0.3,0}, .scale = {1,1,1} },
		},
};
static struct animation dance_animation = {
	.frames_len = 4,
	.frames = dance_animation_frames
};

static void lerp(struct cb_transform *from, struct cb_transform *to, double interpolation, struct cb_transform *lerp) {
	glm_vec3_lerp(from->position, to->position, interpolation, lerp->position);
	glm_vec3_lerp(from->rotation, to->rotation, interpolation, lerp->rotation);
	glm_vec3_lerp(from->scale, to->scale, interpolation, lerp->scale);
}

static void on_update(struct cb_entity *robin, struct cb *cb) {
	struct data *data = robin->data;

	double dist = glm_vec3_distance(robin->transform.position, cb->camera.transform.position);
	if (dist < 40) {
		if (data->animation != &dance_animation) {
			data->animation_frame_time = 0;
			data->animation_frame_number = 0;
			data->animation = &dance_animation;
		}
	} else {
		if (data->animation != &idle_animation) {
			data->animation_frame_time = 0;
			data->animation_frame_number = 0;
			data->animation = &idle_animation;
		}
	}

	int i = (data->animation_frame_number + 0) % data->animation->frames_len;
	int j = (data->animation_frame_number + 1) % data->animation->frames_len;

	struct frame *last_frame = &data->animation->frames[i];
	struct frame *next_frame = &data->animation->frames[j];

	double frame_progress = (double) data->animation_frame_time / last_frame->delay;

	if (frame_progress >= 1) {
		data->animation_frame_number = j;
		data->animation_frame_time = 0;
		i = (data->animation_frame_number + 0) % data->animation->frames_len;
		j = (data->animation_frame_number + 1) % data->animation->frames_len;
		last_frame = &data->animation->frames[i];
		next_frame = &data->animation->frames[j];
		frame_progress = (double) data->animation_frame_time / last_frame->delay;
	}

	if (data->body == NULL) {
		int *i = NULL;
		*i = 10;
	}
	lerp(&last_frame->body, &next_frame->body, frame_progress, &data->body->transform);
	lerp(&last_frame->left_leg, &next_frame->left_leg, frame_progress, &data->left_leg->transform);
	lerp(&last_frame->right_leg, &next_frame->right_leg, frame_progress, &data->right_leg->transform);
	lerp(&last_frame->left_wing, &next_frame->left_wing, frame_progress, &data->left_wing->transform);
	lerp(&last_frame->right_wing, &next_frame->right_wing, frame_progress, &data->right_wing->transform);
	lerp(&last_frame->tail, &next_frame->tail, frame_progress, &data->tail->transform);

	data->animation_frame_time += 1;
}

static void on_stop(struct cb_entity *robin) {
	free(robin->data);
}

struct cb_entity *ww_robin(
		struct cb_entity *parent,
		struct cb_model *body_model, struct cb_texture *body_texture,
		struct cb_model *left_leg_model, struct cb_texture *left_leg_texture,
		struct cb_model *right_leg_model, struct cb_texture *right_leg_texture,
		struct cb_model *left_wing_model, struct cb_texture *left_wing_texture,
		struct cb_model *right_wing_model, struct cb_texture *right_wing_texture,
		struct cb_model *tail_model, struct cb_texture *tail_texture,
		struct cb_shader *shader) {

	struct cb_entity *robin = cb_entity(parent);
	robin->update_fn = on_update;
	robin->stop_fn = on_stop;
	struct data *data = malloc(sizeof *data);
	robin->data = data;

	data->body = cb_entity(robin);
	data->body->model = body_model;
	data->body->texture = body_texture;
	data->body->shader = shader;

	data->left_wing = cb_entity(data->body);
	data->left_wing->model = left_wing_model;
	data->left_wing->texture = left_wing_texture;
	data->left_wing->shader = shader;

	data->right_wing = cb_entity(data->body);
	data->right_wing->model = right_wing_model;
	data->right_wing->texture = right_wing_texture;
	data->right_wing->shader = shader;

	data->tail = cb_entity(data->body);
	data->tail->model = tail_model;
	data->tail->texture = tail_texture;
	data->tail->shader = shader;

	data->left_leg = cb_entity(data->body);
	data->left_leg->model = left_leg_model;
	data->left_leg->texture = left_leg_texture;
	data->left_leg->shader = shader;

	data->right_leg = cb_entity(data->body);
	data->right_leg->model = left_leg_model;
	data->right_leg->texture = left_leg_texture;
	data->right_leg->shader = shader;

	data->animation_frame_time = 0;
	data->animation_frame_number = 0;
	data->animation = &idle_animation;

	return robin;
}
