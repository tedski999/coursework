#include "shader.h"
#include "../platform/alloc.h"
#include "../debug/assert.h"
#include "../debug/prof.h"
#include "wad.h"
#include <glad/gl.h>

static void compile_shader(GLuint program, GLuint shader, struct zip *wad, char *path) {
	cb_prof(CB_PROF_START, "Compile %s", path);

	char *data = cb_wad_load(wad, path, NULL);
	glShaderSource(shader, 1, (const GLchar **) &data, NULL);
	glCompileShader(shader);

	int log_len, compile_success;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &compile_success);
	if (!compile_success) {
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
		char *log = cb_malloc(log_len);
		glGetShaderInfoLog(shader, log_len, NULL, log);
		cb_assert(0, "Unable to compile shader %s: %s", path, log);
	}

	glAttachShader(program, shader);
	glDeleteShader(shader);
	cb_free(data);

	cb_prof(CB_PROF_END, "Compile %s", path);
}

GLuint cb_shader_load(struct zip *wad, char *vert_path, char *frag_path) {
	cb_prof(CB_PROF_START, "Shader %s %s", vert_path, frag_path);
	GLuint program = glCreateProgram();

	cb_prof(CB_PROF_START, "Compiling");
	compile_shader(program, glCreateShader(GL_VERTEX_SHADER), wad, vert_path);
	compile_shader(program, glCreateShader(GL_FRAGMENT_SHADER), wad, frag_path);
	cb_prof(CB_PROF_END, "Compiling");

	cb_prof(CB_PROF_START, "Linking");
	int log_len, link_success, validate_success;
	glLinkProgram(program);
	glGetProgramiv(program, GL_LINK_STATUS, &link_success);
	glValidateProgram(program);
	glGetProgramiv(program, GL_VALIDATE_STATUS, &validate_success);
	if (!link_success || !validate_success) {
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_len);
		char *log = cb_malloc(log_len);
		glGetProgramInfoLog(program, log_len, NULL, log);
		cb_assert(0, "Unable to create shader program:\n%s\n%s\n%s", vert_path, frag_path, log);
	}
	cb_prof(CB_PROF_END, "Linking");

	cb_prof(CB_PROF_END, "Shader %s %s", vert_path, frag_path);
	return program;
}

void cb_shader_unload(GLuint shader) {
	glDeleteProgram(shader);
}
