#ifndef CBI_DATA_SHADER_H
#define CBI_DATA_SHADER_H

#include <glad/gl.h>

struct cbi_wad;

struct cb_shader {
	GLuint program;
	GLuint u_model;
	GLuint u_modelview;
	GLuint u_modelviewproj;
	GLuint u_texture;
};

struct cb_shader *cbi_shader(struct cbi_wad *wad, char *vertname, char *fragname);

#endif
