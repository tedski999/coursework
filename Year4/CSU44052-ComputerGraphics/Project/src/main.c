#include "robin.h"
#include "player.h"
#include <cb.h>

void cb_main(struct cb *cb) {
	cb_window_title(cb, "Winter Wonderland");
	cb->camera.transform.position[vY] = 10;
	cb->camera.transform.position[vZ] = 50;

	// TODO(shader types): choose what type of shader is being loaded - are there types of models and textures?
	struct cb_model *plane_model = cb_model(cb, "res/plane.obj");
	struct cb_model *box_model = cb_model(cb, "res/box.obj");
	struct cb_texture *box_texture = cb_texture(cb, "res/box.png");
	struct cb_model *robin_body_model = cb_model(cb, "res/robin/body.obj");
	struct cb_model *robin_left_leg_model = cb_model(cb, "res/robin/left_leg.obj");
	struct cb_model *robin_right_leg_model = cb_model(cb, "res/robin/right_leg.obj");
	struct cb_model *robin_left_wing_model = cb_model(cb, "res/robin/left_wing.obj");
	struct cb_model *robin_right_wing_model = cb_model(cb, "res/robin/right_wing.obj");
	struct cb_model *robin_tail_model = cb_model(cb, "res/robin/tail.obj");
	struct cb_texture *robin_body_texture = cb_texture(cb, "res/robin/body.png");
	struct cb_texture *robin_left_leg_texture = cb_texture(cb, "res/robin/left_leg.png");
	struct cb_texture *robin_right_leg_texture = cb_texture(cb, "res/robin/right_leg.png");
	struct cb_texture *robin_left_wing_texture = cb_texture(cb, "res/robin/left_wing.png");
	struct cb_texture *robin_right_wing_texture = cb_texture(cb, "res/robin/right_wing.png");
	struct cb_texture *robin_tail_texture = cb_texture(cb, "res/robin/tail.png");
	struct cb_shader *basic_shader = cb_shader(cb, "res/basic.vert.glsl", "res/basic.frag.glsl");
	struct cb_shader *light_shader = cb_shader(cb, "res/light.vert.glsl", "res/light.frag.glsl");

	for (int i = 0; i < 3; i++) {
		struct cb_entity *robin = ww_robin(
				&cb->scene,
				robin_body_model, robin_body_texture,
				robin_left_leg_model, robin_left_leg_texture,
				robin_right_leg_model, robin_right_leg_texture,
				robin_left_wing_model, robin_left_wing_texture,
				robin_right_wing_model, robin_right_wing_texture,
				robin_tail_model, robin_tail_texture,
				light_shader);
		glm_vec3((vec4) { i * 35 - 35, 0, 0 }, robin->transform.position);
		glm_vec3((vec4) { 0, M_PI_2, 0 }, robin->transform.rotation);
		glm_vec3_fill(robin->transform.scale, 10);
	}

	struct cb_entity *box = cb_entity(&cb->scene);
	box->model = box_model;
	box->texture = box_texture;
	box->shader = light_shader;
	glm_vec3_fill(box->transform.scale, 1000);

	struct cb_entity *plane = cb_entity(&cb->scene);
	plane->model = plane_model;
	plane->shader = basic_shader;
	plane->transform.position[vY] = -7.0f;
	glm_vec3_fill(plane->transform.scale, 60);

	ww_player(&cb->scene);
}
