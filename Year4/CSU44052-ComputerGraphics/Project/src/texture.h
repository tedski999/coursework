#ifndef WW_TEXTURE_H
#define WW_TEXTURE_H

#include <glad/gl.h>

GLuint ww_texture_load(char *path);
void ww_texture_unload(GLuint texture);

#endif
