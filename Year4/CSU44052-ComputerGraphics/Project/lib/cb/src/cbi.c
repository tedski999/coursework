#include "cbi.h"
#include "cfg.h"
#include "core/wad.h"
#include "core/window.h"
#include "gfx/entity.h"
#include "gfx/skybox.h"
#include "gfx/transform.h"
#include "util/prof.h"
#include "util/log.h"
#include <string.h>
#include <libgen.h>
#include <glad/gl.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

static void update(struct cbi *cbi) {
	cbi_prof(CBI_PROF_START, "Update");

	cbi_prof(CBI_PROF_START, "Preupdate");
	cbi_transform_update(&cbi->cb.camera.transform);
	cbi_entity_preupdate(&cbi->cb.scene);
	cbi_prof(CBI_PROF_END, "Preupdate");

	if (cbi->cb.input.keys[CB_KEY_F7] == CB_ON_KEY_DOWN)
		cbi_prof_toggle();

	cbi_prof(CBI_PROF_START, "Input");
	for (int i = 0; i < CB_KEY_LAST; i++)
		cbi->cb.input.keys[i] &= 0x1;
	glm_vec2_zero(cbi->cb.input.scroll);
	glm_vec2_copy(cbi->cb.input.mouse, cbi->cb.input.dmouse);
	glfwPollEvents();
	glm_vec2_sub(cbi->cb.input.mouse, cbi->cb.input.dmouse, cbi->cb.input.dmouse);
	cbi_prof(CBI_PROF_END, "Input");

	cbi_prof(CBI_PROF_START, "Entities");
	cbi_entity_update(&cbi->cb.scene, cbi);
	cbi_prof(CBI_PROF_END, "Entities");

	cbi_prof(CBI_PROF_END, "Update");
}

static void render(struct cbi *cbi, double interpolation) {
	cbi_prof(CBI_PROF_START, "Render");

	cbi_prof(CBI_PROF_START, "glClear");
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	cbi_prof(CBI_PROF_END, "glClear");

	mat4 projection;
	glm_perspective(cbi->cb.camera.fov, CB_ASPECT, 0.1f, 10000.0f, projection);

	mat4 origin;
	struct cb_transform camera;
	cbi_transform_lerp(&cbi->cb.camera.transform, interpolation, &camera);
	glm_vec3_mul(camera.position, (vec3){-1,-1, -1}, camera.position);
	glm_vec3_mul(camera.rotation, (vec3){-1, 1, 1}, camera.rotation);
	glm_vec3_mul(camera.scale,    (vec3){ 1, 1, 1}, camera.scale);
	cbi_transform_inverse_matrix(&camera, &origin);

	cbi_prof(CBI_PROF_START, "Entities");
	cbi_entity_render(&cbi->cb.scene, &origin, &projection, interpolation);
	cbi_prof(CBI_PROF_END, "Entities");

	// TODO:skybox
	cbi_skybox_render();

	cbi_window_draw(cbi->window);
	cbi_prof(CBI_PROF_END, "Render");
}

int main(int argc, char **argv) {
	cbi_prof_start();

	cbi_prof(CBI_PROF_START, "Setup");

	cbi_prof(CBI_PROF_START, "Engine");
	cbi_log(CB_LOG_NOTE, "Setting up engine...");
	struct cbi cbi = {
		.cb.is_running = true,
		.cb.camera.fov = M_PI_2,
		.cb.camera.transform.scale = GLM_VEC3_ONE_INIT,
		.cb.scene.transform.scale = GLM_VEC3_ONE_INIT,
		.wad = cbi_wad_open(argv[0]),
		.window = cbi_window_open(&cbi)
	};
	cbi_prof(CBI_PROF_END, "Engine");

	cbi_prof(CBI_PROF_START, "Application");
	cbi_log(CB_LOG_NOTE, "Starting application...");
	cb_main(&cbi.cb);
	cbi_prof(CBI_PROF_END, "Application");

	cbi_prof(CBI_PROF_END, "Setup");

	cbi_log(CB_LOG_NOTE, "Entering main loop...");
	double time = 0, tick = 1.0 / CB_TPS;
	while (cbi.cb.is_running) {
		double deltatime = glfwGetTime() - time;
		tick += fmin(deltatime, CB_MAX_DT);
		time += deltatime;
		while (tick >= 1.0 / CB_TPS) {
			tick -= 1.0 / CB_TPS;
			update(&cbi);
		}
		render(&cbi, tick / (1.0 / CB_TPS));
	}

	cbi_log(CB_LOG_NOTE, "Shutting down...");
	cbi_prof(CBI_PROF_EVENT, "Exit");
	cbi_entity_remove(&cbi.cb.scene);
	cbi_window_close(cbi.window);
	cbi_wad_close(cbi.wad);
	cbi_prof_finish();
	return EXIT_SUCCESS;
}
