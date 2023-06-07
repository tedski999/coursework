#ifndef CB_MODEL_H
#define CB_MODEL_H

#include <glad/gl.h>

struct zip;

struct cb_mesh { GLuint vao, vbo, ebo; int len; };
struct cb_model { struct cb_mesh *meshes; int meshes_len; };

struct cb_model *cb_model_load(struct zip *wad, char *path);
void cb_model_unload(struct cb_model *model);

#endif
