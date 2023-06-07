#include "texture.h"
#include "wad.h"
#include "../platform/alloc.h"
#include <stdlib.h>
#include <glad/gl.h>
#include <png.h>

static void read_png_data(png_structp png_ptr, png_bytep dst, png_size_t len) {
	png_bytepp src_ptr = png_get_io_ptr(png_ptr);
	for (int i = 0; i < len; i++) {
		dst[i] = **src_ptr;
		(*src_ptr)++;
	}
}

GLuint cb_texture_load(struct zip *wad, char *path) {
	char *data = cb_wad_load(wad, path, NULL);
	png_bytep bytes = (png_bytep) data;

	GLuint texture;

	glGenTextures(1, &texture);
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

	int channels = 0;
	GLuint type = 0;
	switch (colortype) {
		case PNG_COLOR_TYPE_RGBA: channels = 4; type = GL_RGBA; break;
		case PNG_COLOR_TYPE_RGB: channels = 3; type = GL_RGB; break;
		default: exit(8);
	}

	png_bytep img = cb_malloc(sizeof *img * height * width * channels);
	for (int i = 0; i < height; i++)
		png_read_row(png_ptr, &img[i * width * channels], NULL);
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

	glBindTexture(GL_TEXTURE_2D, texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexImage2D(GL_TEXTURE_2D, 0, type, width, height, 0, type, GL_UNSIGNED_BYTE, img);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);

	cb_free(data);
	cb_free(img);
	return texture;
}

void cb_texture_unload(GLuint texture) {
	glDeleteTextures(1, &texture);
}
