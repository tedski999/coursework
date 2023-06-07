#ifndef CB_TEXTURE_H
#define CB_TEXTURE_H

#include <glad/gl.h>

struct zip;

GLuint cb_texture_load(struct zip *wad, char *path);
void cb_texture_unload(GLuint texture);

#endif
