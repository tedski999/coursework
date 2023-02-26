#ifndef WW_MODEL_H
#define WW_MODEL_H

#include <glad/gl.h>
#include <cglm/cglm.h>

struct ww_model_mesh_vertex {
	vec3 position;
	vec3 normal;
	vec2 texcoords;
	vec4 bone_weights;
	ivec4 bone_indices;
};

struct ww_model_mesh {
	GLuint vao, vbo, ebo;
	GLuint *indices;
	struct ww_model_mesh_vertex *vertices;
	int indices_len;
	int vertices_len;
};

struct ww_model_animation_channel_key {
	mat4 transform;
	double time;
};

struct ww_model_animation_channel {
	char *name;
	struct ww_model_animation_channel_key *keys;
	int keys_len;
};

struct ww_model_animation {
	char *name;
	double duration;
	double tps;
	struct ww_model_animation_channel *channels;
	int channels_len;
};

struct ww_model_bone {
	char *name;
	mat4 current;
	mat4 offset;
};

struct ww_model_node {
	char *name;
	mat4 transform;
	struct ww_model_node *children;
	int children_len;
};

struct ww_model {
	struct ww_model_node root_node;
	struct ww_model_mesh *meshes;
	struct ww_model_animation *animations;
	struct ww_model_bone *bones;
	int meshes_len;
	int animations_len;
	int bones_len;
	mat4 inverse_transform;
};

struct ww_model *ww_model_load(char *path, char *type);
void ww_model_render(struct ww_model *model);
void ww_model_unload(struct ww_model *model);

#endif
