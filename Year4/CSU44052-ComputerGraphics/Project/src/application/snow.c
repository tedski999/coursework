#include "snow.h"
#include "../alloc.h"
#include "../config.h"
#include "../entity.h"

extern struct ww_entity *camera;

struct particle {
	struct ww_transform transform;
	struct ww_transform transform_old;
	vec3 velocity, angular;
	double lifetime;
};

struct data {
	GLuint shader, u_transform;
	GLuint vao, triangle_vbo, instances_vbo;
	struct particle *particles;
	int particles_len;
	int spawn_rate;
};

static double randr(double min, double max) {
	return (rand() * (max - min)) / RAND_MAX + min;
}

static void on_update(struct ww_entity *snow) {
	struct data *data = snow->data;

	for (int i = 0; i < data->particles_len; i++) {
		struct particle *p = &data->particles[i];
		if (p->lifetime <= 0) continue;
		p->lifetime -= WW_DT;

		glm_vec3_copy(p->transform.position, p->transform_old.position);
		glm_vec3_copy(p->transform.rotation, p->transform_old.rotation);
		glm_vec3_copy(p->transform.scale,    p->transform_old.scale);

		// TODO: do things like wind with velocity

		glm_vec3_add(p->transform.position, p->velocity, p->transform.position);
		glm_vec3_add(p->transform.rotation, p->angular, p->transform.rotation);
	}

	for (int i = 0; i < data->spawn_rate; i++) {
		struct particle *p = NULL;
		// TODO(optimisation): keep track of last j used
		for (int j = 0; j < data->particles_len && !p; j++)
			if (data->particles[j].lifetime <= 0)
				p = &data->particles[j];
		if (!p) break;
		glm_vec3_copy((vec3) { randr(-10,10),   randr(-10,10),   randr(-10,10) },   p->transform.position);
		glm_vec3_copy((vec3) { randr(0,M_PI*2), randr(0,M_PI*2), randr(0,M_PI*2) }, p->transform.rotation);
		glm_vec3_copy((vec3) { 100,             100,             100 },             p->transform.scale);
		glm_vec3_copy(p->transform.position, p->transform_old.position);
		glm_vec3_copy(p->transform.rotation, p->transform_old.rotation);
		glm_vec3_copy(p->transform.scale,    p->transform_old.scale);
		// glm_vec3_copy((vec3) { randr(-10,10),       randr(-10,10),       randr(-10,10) },       p->velocity);
		glm_vec3_zero(p->velocity);
		glm_vec3_copy((vec3) { randr(-0.01,0.01),   randr(-0.01,0.01),   randr(-0.01,0.01) },   p->angular);
		p->lifetime = 1;
	}
}

static void on_render(struct ww_entity *snow, struct ww_transforms *transforms, double interp) {
	struct data *data = snow->data;

	mat4 instance_transforms[data->particles_len];
	for (int i = 0; i < data->particles_len; i++) {
		struct particle *p = &data->particles[i];
		if (p->lifetime <= 0) {
			glm_scale_make(instance_transforms[i], GLM_VEC3_ZERO);
			continue;
		}
		// Interpolate transformation for current frame
		struct ww_transform lerp;
		glm_vec3_lerpc(p->transform_old.position, p->transform.position, interp, lerp.position);
		glm_vec3_lerpc(p->transform_old.rotation, p->transform.rotation, interp, lerp.rotation);
		glm_vec3_lerpc(p->transform_old.scale,    p->transform.scale,    interp, lerp.scale);
		// Compute current transform
		glm_mat4_identity(instance_transforms[i]);
		glm_scale(instance_transforms[i], lerp.scale);
		glm_rotate_z(instance_transforms[i], lerp.rotation[vZ], instance_transforms[i]);
		glm_rotate_y(instance_transforms[i], lerp.rotation[vY], instance_transforms[i]);
		glm_rotate_x(instance_transforms[i], lerp.rotation[vX], instance_transforms[i]);
		glm_translate(instance_transforms[i], lerp.position);
	}
	// Send to GPU
	glBindBuffer(GL_ARRAY_BUFFER, data->instances_vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof instance_transforms, instance_transforms, GL_STREAM_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	glUseProgram(data->shader);
	glUniformMatrix4fv(data->u_transform, 1, GL_FALSE, *transforms->mvp);
	glBindVertexArray(data->vao);
    glDrawArraysInstanced(GL_TRIANGLES, 0, 3, data->particles_len);
	glBindVertexArray(0);
	glUseProgram(0);
}

static void on_destroy(struct ww_entity *snow) {
	struct data *data = snow->data;
	ww_free(data->particles);
	glDeleteBuffers(1, &data->instances_vbo);
	glDeleteBuffers(1, &data->triangle_vbo);
	glDeleteVertexArrays(1, &data->vao);
	ww_free(snow->data);
}

struct ww_entity *ww_snow(GLuint shader) {
	struct ww_entity *snow = ww_entity_create();
	struct data *data = ww_malloc(sizeof *data);

	snow->update_fn = on_update;
	snow->render_fn = on_render;
	snow->cleanup_fn = on_destroy;

	snow->data = data;
	*data = (struct data) { shader, glGetUniformLocation(shader, "u_transform"), };

	data->spawn_rate = 10;
	data->particles_len = 1000;
	data->particles = ww_calloc(data->particles_len, sizeof *data->particles);

	glGenVertexArrays(1, &data->vao);
	glBindVertexArray(data->vao);

	enum { POSITION, TRANSFORM0, TRANSFORM1, TRANSFORM2, TRANSFORM3 };

	glGenBuffers(1, &data->triangle_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, data->triangle_vbo);

	GLfloat triangle_vertices[] = { -1,1,0, 0,-1,0, 1,1,0 };
	glBufferData(GL_ARRAY_BUFFER, sizeof triangle_vertices, triangle_vertices, GL_STATIC_DRAW);
    glEnableVertexAttribArray(POSITION);
	glVertexAttribPointer(POSITION, 3, GL_FLOAT, GL_FALSE, sizeof (GLfloat) * 3, (void *) 0);

	glGenBuffers(1, &data->instances_vbo);
	glBindBuffer(GL_ARRAY_BUFFER, data->instances_vbo);
    glEnableVertexAttribArray(TRANSFORM0);
    glEnableVertexAttribArray(TRANSFORM1);
    glEnableVertexAttribArray(TRANSFORM2);
    glEnableVertexAttribArray(TRANSFORM3);
    glVertexAttribPointer(TRANSFORM0, 4, GL_FLOAT, GL_FALSE, sizeof (mat4), (void *) (sizeof (GLfloat) * 0));
    glVertexAttribPointer(TRANSFORM1, 4, GL_FLOAT, GL_FALSE, sizeof (mat4), (void *) (sizeof (GLfloat) * 3));
    glVertexAttribPointer(TRANSFORM2, 4, GL_FLOAT, GL_FALSE, sizeof (mat4), (void *) (sizeof (GLfloat) * 6));
    glVertexAttribPointer(TRANSFORM3, 4, GL_FLOAT, GL_FALSE, sizeof (mat4), (void *) (sizeof (GLfloat) * 9));
    glVertexAttribDivisor(TRANSFORM0, 1);
    glVertexAttribDivisor(TRANSFORM1, 1);
    glVertexAttribDivisor(TRANSFORM2, 1);
    glVertexAttribDivisor(TRANSFORM3, 1);

    glBindVertexArray(0);

	return snow;
}
