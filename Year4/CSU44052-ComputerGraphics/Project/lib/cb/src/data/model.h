#ifndef CBI_DATA_MODEL_H
#define CBI_DATA_MODEL_H

#include <glad/gl.h>

struct cbi_wad;

struct cbi_mesh {
	GLuint vao;
	GLuint vbo;
	GLuint ebo;
	int len;
};

struct cb_model {
	struct cbi_mesh *meshes;
	int meshes_len;
};

struct cb_model *cbi_model(struct cbi_wad *wad, char *name);

#endif
