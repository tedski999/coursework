#ifndef WW_SHADER_H
#define WW_SHADER_H

#include <glad/gl.h>

GLuint ww_shader_load(char *vert_path, char *frag_path);
void ww_shader_unload(GLuint shader);

#endif
