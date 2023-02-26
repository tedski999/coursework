#include "skybox.h"
#include "../alloc.h"
#include "../entity.h"
#include "../model.h"

extern struct ww_entity *camera;

struct data {
	struct ww_model *model;
	GLuint shader;
	GLuint texture;
	GLuint u_transform;
	GLuint u_texture;
};

static void on_update(struct ww_entity *skybox) {
	glm_vec3_copy(camera->transform.position, skybox->transform.position);
}

static void on_render(struct ww_entity *skybox, struct ww_transforms *transforms, double interp) {
	struct data *data = skybox->data;
	glUseProgram(data->shader);
	glUniformMatrix4fv(data->u_transform, 1, GL_FALSE, *transforms->mvp);
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, data->texture);
	glUniform1i(data->u_texture, 0);
	ww_model_render(data->model);
}

static void on_destroy(struct ww_entity *skybox) {
	ww_free(skybox->data);
}

struct ww_entity *ww_skybox(struct ww_model *model, GLuint shader, GLuint texture) {
	struct ww_entity *skybox = ww_entity_create();
	struct data *data = ww_malloc(sizeof *data);

	skybox->update_fn = on_update;
	skybox->render_fn = on_render;
	skybox->cleanup_fn = on_destroy;

	skybox->data = data;
	*data = (struct data) {
		model, shader, texture,
		glGetUniformLocation(shader, "u_transform"),
		glGetUniformLocation(shader, "u_texture")
	};

	for (int m = 0; m < model->meshes_len; m++) {
		struct ww_model_mesh *mesh = &model->meshes[m];
		int vertices_len = mesh->vertices_len;
		struct vertex { vec3 position; vec2 texcoords; };
		struct vertex vertices[mesh->vertices_len];
		for (int v = 0; v < vertices_len; v++) {
			struct ww_model_mesh_vertex *vertex = &mesh->vertices[v];
			glm_vec3_copy(vertex->position, vertices[v].position);
			glm_vec2_copy(vertex->texcoords, vertices[v].texcoords);
		}
		glBindVertexArray(mesh->vao);
		glBindBuffer(GL_ARRAY_BUFFER, mesh->vbo);
		glBufferData(GL_ARRAY_BUFFER, vertices_len * sizeof *vertices, vertices, GL_STATIC_DRAW);
		enum { POSITION, TEXCOORDS };
		glEnableVertexAttribArray(POSITION);
		glEnableVertexAttribArray(TEXCOORDS);
		glVertexAttribPointer(POSITION,  3, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, position)));
		glVertexAttribPointer(TEXCOORDS, 2, GL_FLOAT, GL_FALSE, sizeof *vertices, (void *) (offsetof(struct vertex, texcoords)));
		glBindVertexArray(0);
	}

	return skybox;
}
