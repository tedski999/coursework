#include "shader.h"
#include "alloc.h"
#include "assert.h"
#include "wad.h"
#include <glad/gl.h>

static void compile_shader(GLuint program, GLuint shader, char *path, char *data) {
	glShaderSource(shader, 1, (const GLchar **) &data, NULL);
	glCompileShader(shader);

	int log_len, compile_success;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &compile_success);
	if (!compile_success) {
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
		char log[log_len];
		glGetShaderInfoLog(shader, log_len, NULL, log);
		ww_assert(0, "Unable to compile shader:\n%s\n%s\n\n%s", log, path, data);
	}

	glAttachShader(program, shader);
	glDeleteShader(shader);
}

GLuint ww_shader_load(char *vert_path, char *frag_path) {
	char *vert_data = ww_wad(vert_path, NULL);
	char *frag_data = ww_wad(frag_path, NULL);

	GLuint program = glCreateProgram();
	compile_shader(program, glCreateShader(GL_VERTEX_SHADER), vert_path, vert_data);
	compile_shader(program, glCreateShader(GL_FRAGMENT_SHADER), frag_path, frag_data);

	int log_len, link_success, validate_success;
	glLinkProgram(program);
	glGetProgramiv(program, GL_LINK_STATUS, &link_success);
	glValidateProgram(program);
	glGetProgramiv(program, GL_VALIDATE_STATUS, &validate_success);
	if (!link_success || !validate_success) {
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_len);
		char log[log_len];
		glGetProgramInfoLog(program, log_len, NULL, log);
		ww_assert(0, "Unable to create shader program:\n%s\n%s\n%s", vert_path, frag_path, log);
	}

	ww_free(vert_data);
	ww_free(frag_data);
	return program;
}

void ww_shader_unload(GLuint shader) {
	glDeleteProgram(shader);
}
