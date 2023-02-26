#include "ground.h"
#include "present.h"
#include "skybox.h"
#include "snow.h"
#include "snowman.h"
#include "tree.h"
#include "candycane.h"
#include "../entity.h"
#include "../model.h"
#include "../shader.h"
#include "../texture.h"
#include <time.h>
#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <assimp/scene.h>

static struct ww_entity *ground;
static GLuint ground_shader;

static struct ww_entity *skybox;
static struct ww_model *skybox_model;
static GLuint skybox_shader;
static GLuint skybox_texture;

static struct ww_entity *snow;
static GLuint snow_shader;

#define presents_len 25
static struct ww_entity *presents[presents_len];
static struct ww_model *present_model;
static GLuint present_shader;
static GLuint present_texture;

#define snowmen_len 5
static struct ww_entity *snowmen[snowmen_len];
static struct ww_model *snowman_model;
static GLuint snowman_shader;

#define trees_len 100
static struct ww_entity *trees[trees_len];
static struct ww_model *tree_model;
static GLuint tree_shader;
static GLuint tree_texture;

#define candycanes_len 5
static struct ww_entity *candycanes[candycanes_len];
static struct ww_model *candycane_model;
static GLuint candycane_shader;
static GLuint candycane_texture;

static vec3 velocity;

extern bool is_running;
extern vec2 mouse, scroll, mouse_diff;
extern int keys[GLFW_KEY_LAST];
extern struct ww_entity *world;
extern struct ww_entity *camera;
extern double camera_fov;

static double randr(double min, double max) {
	return (rand() * (max - min)) / RAND_MAX + min;
}

static void on_update(struct ww_entity *world) {
	camera_fov = glm_clamp(camera_fov * (1 + scroll[vY] * 0.1), 0, M_PI_2);

	double sensitivity = camera_fov * 0.0025;
	camera->transform.rotation[vX] += sensitivity * 10 * (keys[GLFW_KEY_UP]);
	camera->transform.rotation[vX] -= sensitivity * 10 * (keys[GLFW_KEY_DOWN]);
	camera->transform.rotation[vX] = glm_clamp(camera->transform.rotation[vX] - mouse_diff[vY] * sensitivity, -M_PI_2, M_PI_2);
	camera->transform.rotation[vY] -= sensitivity * 10 * (keys[GLFW_KEY_LEFT]);
	camera->transform.rotation[vY] += sensitivity * 10 * (keys[GLFW_KEY_RIGHT]);
	camera->transform.rotation[vY] += mouse_diff[vX] * sensitivity;

	vec3 forward = { sin(camera->transform.rotation[vY]) * cos(camera->transform.rotation[vX]), sin(camera->transform.rotation[vX]), cos(camera->transform.rotation[vY]) * -cos(camera->transform.rotation[vX])};
	vec3 up =      { 0, 1, 0};
	vec3 right =   { cos(-camera->transform.rotation[vY]), 0, -sin(-camera->transform.rotation[vY])};

	vec3 direction = GLM_VEC3_ZERO_INIT;
	if (keys[GLFW_KEY_W])          glm_vec3_add(direction, forward, direction);
	if (keys[GLFW_KEY_S])          glm_vec3_sub(direction, forward, direction);
	if (keys[GLFW_KEY_D])          glm_vec3_add(direction, right, direction);
	if (keys[GLFW_KEY_A])          glm_vec3_sub(direction, right, direction);
	if (keys[GLFW_KEY_SPACE])      glm_vec3_add(direction, up, direction);
	if (keys[GLFW_KEY_LEFT_SHIFT]) glm_vec3_sub(direction, up, direction);
	if (keys[GLFW_KEY_LEFT_CONTROL]) {
		glm_vec3_scale(velocity, 0.9, velocity);
		if (fabs(velocity[vX]) + fabs(velocity[vY]) + fabs(velocity[vZ])  < 0.05)
			glm_vec3_zero(velocity);

	}
	glm_normalize(direction);

	double speed = 0.02;
	vec3 acceleration = GLM_VEC3_ZERO_INIT;
	glm_vec3_scale(direction, speed, acceleration);
	glm_vec3_add(velocity, acceleration, velocity);

	glm_vec3_add(camera->transform.position, velocity, camera->transform.position);

	if (keys[GLFW_KEY_ESCAPE]) {
		is_running = false;
	}
}

void ww_setup(void) {
	srand(time(NULL));

	world->update_fn = on_update;
	camera->transform.position[vY] = 10;
	camera->transform.position[vZ] = 10;

	ground_shader = ww_shader_load("res/ground.vert.glsl", "res/ground.frag.glsl");

	skybox_model = ww_model_load("res/skybox.glb", "glb");
	skybox_shader = ww_shader_load("res/skybox.vert.glsl", "res/skybox.frag.glsl");
	skybox_texture = ww_texture_load("res/skybox.png");

	snow_shader = ww_shader_load("res/snow.vert.glsl", "res/snow.frag.glsl");

	present_model = ww_model_load("res/present.glb", "glb");
	present_shader = ww_shader_load("res/present.vert.glsl", "res/present.frag.glsl");
	present_texture = ww_texture_load("res/present.png");

	snowman_model = ww_model_load("res/snowman.glb", "glb");
	snowman_shader = ww_shader_load("res/snowman.vert.glsl", "res/snowman.frag.glsl");

	tree_model = ww_model_load("res/tree.glb", "glb");
	tree_shader = ww_shader_load("res/tree.vert.glsl", "res/tree.frag.glsl");
	tree_texture = ww_texture_load("res/tree.png");

	candycane_model = ww_model_load("res/candycane.glb", "glb");
	candycane_shader = ww_shader_load("res/candycane.vert.glsl", "res/candycane.frag.glsl");
	candycane_texture = ww_texture_load("res/candycane.png");

	ground = ww_ground(rand(), 512, 0.05, 7, 250, ground_shader);
	ww_entity_attach(world, ground);

	skybox = ww_skybox(skybox_model, skybox_shader, skybox_texture);
	glm_vec3_copy((vec3){0,0,0}, skybox->transform.rotation);
	glm_vec3_copy((vec3){1000,1000,1000}, skybox->transform.scale);
	ww_entity_attach(world, skybox);

	snow = ww_snow(snow_shader);
	ww_entity_attach(world, snow);

	for (int i = 0; i < presents_len; i++) {
		struct ww_entity *present = ww_present(present_model, present_shader, present_texture);
		ww_entity_attach(world, present);
		double radius = randr(0, 80);
		double angle = randr(0, M_PI*2);
		present->transform.position[vX] = cos(angle) * radius;
		present->transform.position[vZ] = sin(angle) * radius;
		present->transform.position[vY] = ww_ground_get_height(ground, present->transform.position[vZ], present->transform.position[vX]) + 0.8;
		present->transform.rotation[vY] = randr(0, M_PI*2);
		present->transform.scale[vX] = present->transform.scale[vY] = present->transform.scale[vZ] = randr(0.9, 1.2);
		presents[i] = present;
	}

	for (int i = 0; i < snowmen_len; i++) {
		struct ww_entity *snowman = ww_snowman(snowman_model, snowman_shader, ground, presents, presents_len);
		ww_entity_attach(world, snowman);
		double radius = randr(0, 80);
		double angle = randr(0, M_PI*2);
		snowman->transform.position[vX] = cos(angle) * radius;
		snowman->transform.position[vZ] = sin(angle) * radius;
		snowman->transform.position[vY] = ww_ground_get_height(ground, snowman->transform.position[vX], snowman->transform.position[vZ]);
		snowman->transform.rotation[vY] = randr(0, M_PI*2);
		snowman->transform.scale[vX] = snowman->transform.scale[vY] = snowman->transform.scale[vZ] = 2;
		snowmen[i] = snowman;
	}

	for (int i = 0; i < trees_len; i++) {
		struct ww_entity *tree = ww_tree(tree_model, tree_shader, tree_texture);
		ww_entity_attach(world, tree);
		double radius = randr(90, 110);
		double angle = (double) i / trees_len * M_PI*2;
		tree->transform.position[vX] = cos(angle) * radius;
		tree->transform.position[vZ] = sin(angle) * radius;
		tree->transform.position[vY] = ww_ground_get_height(ground, tree->transform.position[vZ], tree->transform.position[vX]);
		tree->transform.rotation[vY] = randr(0, M_PI*2);
		tree->transform.scale[vX] = tree->transform.scale[vY] = tree->transform.scale[vZ] = randr(1.8, 2.2);
		trees[i] = tree;
	}

	for (int i = 0; i < candycanes_len; i++) {
		struct ww_entity *candycane = ww_candycane(candycane_model, candycane_shader, candycane_texture);
		ww_entity_attach(world, candycane);
		double radius = randr(45, 70);
		double angle = (double) i / candycanes_len * M_PI*2;
		candycane->transform.position[vX] = cos(angle) * radius;
		candycane->transform.position[vZ] = sin(angle) * radius;
		candycane->transform.position[vY] = ww_ground_get_height(ground, candycane->transform.position[vX], candycane->transform.position[vZ]) - randr(0, 3);
		candycane->transform.rotation[vY] = M_PI_2-angle + randr(-M_PI/8, M_PI/8);
		candycanes[i] = candycane;
	}
}

void ww_shutdown(void) {
	ww_entity_destroy(ground);
	ww_shader_unload(ground_shader);

	ww_entity_destroy(snow);
	ww_shader_unload(snow_shader);

	ww_entity_destroy(skybox);
	ww_model_unload(skybox_model);
	ww_shader_unload(skybox_shader);
	ww_texture_unload(skybox_texture);

	for (int i = 0; i < presents_len; i++)
		ww_entity_destroy(presents[i]);
	ww_model_unload(present_model);
	ww_shader_unload(present_shader);
	ww_texture_unload(present_texture);

	for (int i = 0; i < snowmen_len; i++)
		ww_entity_destroy(snowmen[i]);
	ww_model_unload(snowman_model);
	ww_shader_unload(snowman_shader);

	for (int i = 0; i < trees_len; i++)
		ww_entity_destroy(trees[i]);
	ww_model_unload(tree_model);
	ww_shader_unload(tree_shader);
	ww_texture_unload(tree_texture);

	for (int i = 0; i < candycanes_len; i++)
		ww_entity_destroy(candycanes[i]);
	ww_model_unload(candycane_model);
	ww_shader_unload(candycane_shader);
	ww_texture_unload(candycane_texture);
}
