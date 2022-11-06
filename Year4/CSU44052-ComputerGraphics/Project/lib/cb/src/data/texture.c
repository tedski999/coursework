#include "texture.h"
#include "../core/wad.h"
#include "../util/alloc.h"
#include <stdlib.h>
#include <png.h>

// TODO(wad): provide function to free loaded data
// TODO: error detection and generalising
// - handle non-RGB images
// - user-specified texture parameters
// glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, texture->width, texture->height, 0, GL_RGBA, GL_UNSIGNED_BYTE, img);

static void read_png_data(png_structp png_ptr, png_bytep dst, png_size_t len) {
	png_bytepp src_ptr = png_get_io_ptr(png_ptr);
	for (int i = 0; i < len; i++) {
		dst[i] = **src_ptr;
		(*src_ptr)++;
	}
}

struct cb_texture *cbi_texture(struct cbi_wad *wad, char *name) {
	char *data = cbi_wad_load(wad, name, NULL);
	png_bytep bytes = (png_bytep) data;

	struct cb_texture *texture = cbi_malloc(sizeof *texture);

	glGenTextures(1, &texture->id);
	if (png_sig_cmp(bytes, 0, 8)) {
		exit(5);
	}

	png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	png_infop info_ptr = png_create_info_struct(png_ptr);
	png_set_read_fn(png_ptr, &bytes, read_png_data);
	if (setjmp(png_jmpbuf(png_ptr))) {
		exit(7);
	}

	int colortype;
	png_uint_32 width, height;
	png_read_info(png_ptr, info_ptr);
	png_get_IHDR(png_ptr, info_ptr, &width, &height, NULL, &colortype, NULL, NULL, NULL);

	texture->width = width;
	texture->height = height;
	switch (colortype) {
		case PNG_COLOR_TYPE_RGBA: texture->channels = 4; break;
		case PNG_COLOR_TYPE_RGB: texture->channels = 3; break;
		default: exit(8);
	}

	png_bytep img = cbi_malloc(sizeof *img * texture->height * texture->width * texture->channels);
	for (int i = 0; i < texture->height; i++)
		png_read_row(png_ptr, &img[i * texture->width * texture->channels], NULL);
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

	glBindTexture(GL_TEXTURE_2D, texture->id);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texture->width, texture->height, 0, GL_RGB, GL_UNSIGNED_BYTE, img);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);

	cbi_free(data);
	cbi_free(img);
	return texture;
}
