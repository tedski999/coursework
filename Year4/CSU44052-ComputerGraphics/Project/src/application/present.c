#include "present.h"
#include "../alloc.h"
#include "../entity.h"
#include "../model.h"

struct data {
	struct ww_present_data public;
	struct ww_model *model;
	GLuint shader;
	GLuint texture;
	GLuint u_transform;
	GLuint u_texture;
	GLuint u_white_color;
	GLuint u_black_color;
	vec3 white_color, black_color;
};

static double randr(double min, double max) {
	return (rand() * (max - min)) / RAND_MAX + min;
}

static void on_render(struct ww_entity *present, struct ww_transforms *transforms, double interp) {
	struct data *data = present->data;
	glUseProgram(data->shader);
	glUniformMatrix4fv(data->u_transform, 1, GL_FALSE, *transforms->mvp);
	glUniformMatrix4fv(glGetUniformLocation(data->shader, "u_model"), 1, GL_FALSE, *transforms->m);
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, data->texture);
	glUniform1i(data->u_texture, 0);
	glUniform4f(data->u_black_color, data->black_color[vX], data->black_color[vY], data->black_color[vZ], 1);
	glUniform4f(data->u_white_color, data->white_color[vX], data->white_color[vY], data->white_color[vZ], 1);
	ww_model_render(data->model);
}

static void on_destroy(struct ww_entity *present) {
	ww_free(present->data);
}

struct ww_entity *ww_present(struct ww_model *model, GLuint shader, GLuint texture) {
	struct ww_entity *present = ww_entity_create();
	struct data *data = ww_malloc(sizeof *data);

	GLuint u_transform = glGetUniformLocation(shader, "u_transform");
	GLuint u_texture = glGetUniformLocation(shader, "u_texture");
	GLuint u_white_color = glGetUniformLocation(shader, "u_white_color");
	GLuint u_black_color = glGetUniformLocation(shader, "u_black_color");

	present->render_fn = on_render;
	present->cleanup_fn = on_destroy;

	present->data = data;
	*data = (struct data) {
		{NULL}, model, shader, texture,
		u_transform, u_texture,
		u_white_color, u_black_color,
		{ randr(0.25, 1.0), randr(0.25, 1.0), randr(0.25, 1.0) },
		{ randr(0.25, 1.0), randr(0.25, 1.0), randr(0.25, 1.0) }
	};

	for (int m = 0; m < model->meshes_len; m++) {
		struct ww_model_mesh *mesh = &model->meshes[m];
		int vertices_len = mesh->vertices_len;
		struct vertex { vec3 position; vec3 normal; vec2 texcoords; };
		struct vertex vertices[mesh->vertices_len];
		for (int v = 0; v < vertices_len; v++) {
			struct ww_model_mesh_vertex *vertex = &mesh->vertices[v];
			glm_vec3_copy(vertex->position, vertices[v].position);
			glm_vec3_copy(vertex->normal, vertices[v].normal);
			glm_vec2_copy(vertex->texcoords, vertices[v].texcoords);
		}
		glBindVertexArray(mesh->vao);
		glBindBuffer(GL_ARRAY_BUFFER, mesh->vbo);
		glBufferData(GL_ARRAY_BUFFER, vertices_len * sizeof *vertices, vertices, GL_STATIC_DRAW);
		enum { POSITION, NORMAL, TEXCOORDS };
		glEnableVertexAttribArray(POSITION);
		glEnableVertexAttribArray(NORMAL);
		glEnableVertexAttribArray(TEXCOORDS);
		glVertexAttribPointer(POSITION,  3, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, position)));
		glVertexAttribPointer(NORMAL,    3, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, normal)));
		glVertexAttribPointer(TEXCOORDS, 2, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, texcoords)));
		glBindVertexArray(0);
	}

	return present;
}
