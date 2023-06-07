// #include "snowman.h"
#include "../core.h"
#include "../scene/entity.h"
#include "../data/model.h"
#include "../data/shader.h"
#include "../data/texture.h"
#include <glad/gl.h>
#include <cglm/cglm.h>

static struct cb_entity snowman;

static struct cb_model *model;
static GLuint shader;
static GLuint texture;

void update(struct cb_entity *entity, struct cb *cb) {
	snowman.transform.rotation[vZ] += 0.01;
}

void render(struct cb_entity *entity, mat4 m, mat4 mv, double i) {
	glUseProgram(shader);

	mat4 projection;
	glm_perspective(M_PI_2, CB_ASPECT, 0.1f, 10000.0f, projection);

	mat4 mvp;
	glm_mat4_mul(projection, mv, mvp);

	GLuint loc = glGetUniformLocation(shader, "u_modelviewproj");
	glUniformMatrix4fv(loc, 1, GL_FALSE, &mvp[0][0]);

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, texture);
	GLuint tloc = glGetUniformLocation(shader, "u_texture");
	glUniform1i(tloc, 0);

	for (int i = 0; i < model->meshes_len; i++) {
		glBindVertexArray(model->meshes[i].vao);
		glDrawElements(GL_TRIANGLES, model->meshes[i].len, GL_UNSIGNED_INT, 0);
	}
}

void cb_start(struct cb *cb) {
	model = cb_model_load(cb->wad, "res/snowman.obj");
	shader = cb_shader_load(cb->wad, "res/snowman.vert.glsl", "res/snowman.frag.glsl");
	texture = cb_texture_load(cb->wad, "res/snowman.png");
	glm_vec3_copy(GLM_VEC3_ONE, snowman.transform.scale);
	snowman.transform.position[vZ] = -1;
	snowman.update_fn = update;
	snowman.render_fn = render;
	cb_entity_add_child(&cb->scene, &snowman);
}

void cb_stop(struct cb *cb) {
}
