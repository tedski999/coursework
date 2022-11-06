#ifndef CBI_DATA_TEXTURE_H
#define CBI_DATA_TEXTURE_H

#include <glad/gl.h>

struct cbi_wad;

struct cb_texture {
	GLuint id;
	int width;
	int height;
	int channels;
};

struct cb_texture *cbi_texture(struct cbi_wad *wad, char *name);

#endif
