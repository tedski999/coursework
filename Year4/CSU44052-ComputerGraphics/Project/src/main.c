#include "alloc.h"
#include "assert.h"
#include "config.h"
#include "log.h"
#include "prof.h"
#include "model.h"
#include "entity.h"
#include "wad.h"
#include "window.h"
#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <cglm/cglm.h>

GLFWwindow *window;
struct zip *wad;
int keys[GLFW_KEY_LAST];
vec2 mouse, mouse_diff, scroll;
bool is_running = true;
struct ww_entity *world;
struct ww_entity *camera;
double camera_fov = 75.0;

extern void ww_setup();
extern void ww_shutdown();

static void update(void) {
	ww_prof(WW_PROF_START, "Update");

	glm_vec2_zero(scroll);
	glm_vec2_copy(mouse, mouse_diff);
	glfwPollEvents();
	glm_vec2_sub(mouse, mouse_diff, mouse_diff);

	ww_entity_preupdate(camera);
	ww_entity_preupdate(world);
	ww_entity_update(camera);
	ww_entity_update(world);

	ww_prof(WW_PROF_END, "Update");
}

static void render(double interp) {
	ww_prof(WW_PROF_START, "Render");

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	struct ww_transform lerp;
	glm_vec3_lerpc(camera->transform_old.position, camera->transform.position, interp, lerp.position);
	glm_vec3_lerpc(camera->transform_old.rotation, camera->transform.rotation, interp, lerp.rotation);
	glm_vec3_lerpc(camera->transform_old.scale, camera->transform.scale, interp, lerp.scale);
	glm_vec3_mul(lerp.position, (vec3){-1,-1,-1}, lerp.position);
	glm_vec3_mul(lerp.rotation, (vec3){-1, 1, 1}, lerp.rotation);
	glm_vec3_mul(lerp.scale,    (vec3){ 1, 1, 1}, lerp.scale);

	struct ww_transforms transforms;
	glm_scale_make(transforms.v, lerp.scale);
	glm_rotate_x(transforms.v, lerp.rotation[vX], transforms.v);
	glm_rotate_y(transforms.v, lerp.rotation[vY], transforms.v);
	glm_rotate_z(transforms.v, lerp.rotation[vZ], transforms.v);
	glm_translate(transforms.v, lerp.position);
	glm_perspective(camera_fov, WW_ASPECT, 0.1f, 10000.0f, transforms.p);
	ww_entity_render(world, transforms, interp);

	// TODO: ui

	ww_prof(WW_PROF_END, "Render");
}

int main(int argc, char **argv) {
	ww_assert(argc > 0 && argv[0], "OS error: Missing executable path from argument list");

	ww_log(WW_LOG_NOTE, "Setting up...");
	ww_prof(WW_PROF_START, "Setup");
	ww_window_open("Winter Wonderland");
	ww_wad_open(argv[0]);
	world = ww_entity_create();
	camera = ww_entity_create();
	ww_setup();
	ww_prof(WW_PROF_END, "Setup");

	ww_log(WW_LOG_NOTE, "Entering main loop...");
	double time = glfwGetTime(), tick = WW_DT;
	while (is_running) {
		double t = glfwGetTime();
		tick += fmin(t - time, WW_MAX_DT);
		time = t;
		while (tick >= WW_DT) {
			tick -= WW_DT;
			update();
		}
		render(tick / WW_DT);
		glfwSwapBuffers(window);
	}

	ww_prof(WW_PROF_EVENT, "Cleanup");
	ww_log(WW_LOG_NOTE, "Shutting down...");
	ww_shutdown();
	ww_entity_destroy(camera);
	ww_entity_destroy(world);
	ww_window_close();
	ww_wad_close();
	// fprintf(stderr, "]}\n");
	return EXIT_SUCCESS;
}
