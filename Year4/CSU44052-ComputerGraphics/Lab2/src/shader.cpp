#include "shader.hpp"
#include <iostream>
#include <fstream>
#include <GL/glew.h>
#include <glm/glm.hpp>

yar::shader::shader(std::string vert_file, std::string frag_file) {
	GLint success = 0;
	GLchar error_buffer[1024];

	std::ifstream vert_file_ifs(vert_file);
	std::string vert_source((std::istreambuf_iterator<char>(vert_file_ifs)), std::istreambuf_iterator<char>());
	std::ifstream frag_file_ifs(frag_file);
	std::string frag_source((std::istreambuf_iterator<char>(frag_file_ifs)), std::istreambuf_iterator<char>());

	// TODO: handle load failure

	program = glCreateProgram();
	if (!program) {
		std::cerr << "Error creating shader program!" << std::endl;
		exit(1);
	}

	auto add_shader = [&](const char *source, GLenum type) {
		GLuint object = glCreateShader(type);
		if (!object) {
			std::cerr << "Error creating shader type " << type << std::endl;
			exit(1);
		}

		glShaderSource(object, 1, (const GLchar **) &source, NULL);
		glCompileShader(object);

		glGetShaderiv(object, GL_COMPILE_STATUS, &success);
		if (!success) {
			glGetShaderInfoLog(object, 1024, NULL, error_buffer);
			std::cerr << "Error compiling shader type " << type << std::endl << error_buffer;
			exit(1);
		}

		glAttachShader(program, object);
		glDeleteShader(object);
	};

	add_shader(vert_source.c_str(), GL_VERTEX_SHADER);
	add_shader(frag_source.c_str(), GL_FRAGMENT_SHADER);
	glLinkProgram(program);

	glGetProgramiv(program, GL_LINK_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(program, sizeof error_buffer, NULL, error_buffer);
		std::cerr << "Error linking shader program: " << error_buffer << std::endl;
		exit(1);
	}

	glValidateProgram(program);
	glGetProgramiv(program, GL_VALIDATE_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(program, sizeof error_buffer, NULL, error_buffer);
		std::cerr << "Invalid shader program " << error_buffer << std::endl;
		exit(1);
	}
}

yar::shader::~shader() {
	glDeleteProgram(program);
}

void yar::shader::set_uniform(std::string name, glm::mat4 value) {
	glUseProgram(program);
	glUniformMatrix4fv(glGetUniformLocation(program, name.c_str()), 1, GL_FALSE, &value[0][0]);
}

void yar::shader::use() {
	glUseProgram(program);
}
