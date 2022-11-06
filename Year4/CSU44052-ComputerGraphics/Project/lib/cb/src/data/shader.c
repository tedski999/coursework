#include "shader.h"
#include "../core/wad.h"
#include "../util/alloc.h"
#include "../util/assert.h"
#include "../util/prof.h"
#include <stdlib.h>
#include <glad/gl.h>

void compile_shader(GLuint program, GLuint shader, struct cbi_wad *wad, char *name) {
	cbi_prof(CBI_PROF_START, "Compile %s", name);

	char *data = cbi_wad_load(wad, name, NULL);
	glShaderSource(shader, 1, (const GLchar **) &data, NULL);
	glCompileShader(shader);

	int log_len, compile_success;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &compile_success);
	if (!compile_success) {
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
		char *log = cbi_malloc(log_len);
		glGetShaderInfoLog(shader, log_len, NULL, log);
		cbi_assert(0, "Unable to compile shader %s: %s", name, log);
	}

	glAttachShader(program, shader);
	glDeleteShader(shader);
	cbi_free(data);

	cbi_prof(CBI_PROF_END, "Compile %s", name);
}

struct cb_shader *cbi_shader(struct cbi_wad *wad, char *vertname, char *fragname) {
	cbi_prof(CBI_PROF_START, "Shader %s %s", vertname, fragname);
	struct cb_shader *shader = cbi_malloc(sizeof *shader);
	shader->program = glCreateProgram();

	cbi_prof(CBI_PROF_START, "Compiling");
	compile_shader(shader->program, glCreateShader(GL_VERTEX_SHADER), wad, vertname);
	compile_shader(shader->program, glCreateShader(GL_FRAGMENT_SHADER), wad, fragname);
	cbi_prof(CBI_PROF_END, "Compiling");

	cbi_prof(CBI_PROF_START, "Linking");
	int log_len, link_success, validate_success;
	glLinkProgram(shader->program);
	glGetProgramiv(shader->program, GL_LINK_STATUS, &link_success);
	glValidateProgram(shader->program);
	glGetProgramiv(shader->program, GL_VALIDATE_STATUS, &validate_success);
	if (!link_success || !validate_success) {
		glGetProgramiv(shader->program, GL_INFO_LOG_LENGTH, &log_len);
		char *log = cbi_malloc(log_len);
		glGetProgramInfoLog(shader->program, log_len, NULL, log);
		cbi_assert(0, "Unable to create shader program:\n%s\n%s\n%s", vertname, fragname, log);
	}
	cbi_prof(CBI_PROF_END, "Linking");

	// TODO(shader types): adapt to the type of shader
	shader->u_texture = glGetUniformLocation(shader->program, "u_texture");
	shader->u_model = glGetUniformLocation(shader->program, "u_model");
	shader->u_modelview = glGetUniformLocation(shader->program, "u_modelview");
	shader->u_modelviewproj = glGetUniformLocation(shader->program, "u_modelviewproj");

	cbi_prof(CBI_PROF_END, "Shader %s %s", vertname, fragname);
	return shader;
}
