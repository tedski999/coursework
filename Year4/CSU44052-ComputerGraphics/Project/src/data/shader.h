#ifndef CB_SHADER_H
#define CB_SHADER_H

#include <glad/gl.h>

struct zip;

GLuint cb_shader_load(struct zip *wad, char *vert_path, char *frag_path);
void cb_shader_unload(GLuint shader);

#endif

