// Ted Johnson <edjohnso@tcd.ie> TCD 19335618

#include <GL/glew.h>
#include <GL/freeglut.h>
#include <iostream>

static const char *g_vert_shader = "     \n\
#version 330                             \n\
layout (location = 0) in vec3 vPosition; \n\
layout (location = 1) in vec4 vColor;    \n\
out vec4 color;                          \n\
void main() {                            \n\
	gl_Position = vec4(vPosition, 1.0);  \n\
	color = vColor;                      \n\
}";

static const char *g_right_frag_shader = "\n\
#version 330                              \n\
in vec4 color;                            \n\
out vec4 FragColor;                       \n\
void main() {                             \n\
	FragColor = color;                    \n\
}";

static const char *g_left_frag_shader = " \n\
#version 330                              \n\
in vec4 color;                            \n\
out vec4 FragColor;                       \n\
void main() {                             \n\
	FragColor = vec4(1.0, 1.0, 0.0, 1.0); \n\
}";

static GLuint g_right_program_id, g_left_program_id;
static GLuint g_right_vao_id, g_left_vao_id;

static void add_shader(GLuint shader_program, const char *source, GLenum shader_type) {
	GLuint shader_object = glCreateShader(shader_type);

	if (!shader_object) {
		fprintf(stderr, "Error creating shader type %d\n", shader_type);
		exit(0);
	}

	glShaderSource(shader_object, 1, (const GLchar **) &source, NULL);
	glCompileShader(shader_object);

	GLint success;
	glGetShaderiv(shader_object, GL_COMPILE_STATUS, &success);
	if (!success) {
		GLchar error_buffer[1024];
		glGetShaderInfoLog(shader_object, 1024, NULL, error_buffer);
		fprintf(stderr, "Error compiling shader type %d: '%s'\n", shader_type, error_buffer);
		exit(1);
	}

	glAttachShader(shader_program, shader_object);
}

static GLuint compile_shader(const char *vert_shader, const char *frag_shader) {
	GLuint shader_program = glCreateProgram();
	if (!shader_program) {
		fprintf(stderr, "Error creating shader program\n");
		exit(1);
	}

	add_shader(shader_program, vert_shader, GL_VERTEX_SHADER);
	add_shader(shader_program, frag_shader, GL_FRAGMENT_SHADER);
	glLinkProgram(shader_program);

	GLint success = 0;
	GLchar error_buffer[1024] = {0};

	glGetProgramiv(shader_program, GL_LINK_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(shader_program, sizeof error_buffer, NULL, error_buffer);
		fprintf(stderr, "Error linking shader program: '%s'\n", error_buffer);
		exit(1);
	}

	glValidateProgram(shader_program);
	glGetProgramiv(shader_program, GL_VALIDATE_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(shader_program, sizeof error_buffer, NULL, error_buffer);
		fprintf(stderr, "Invalid shader program: '%s'\n", error_buffer);
		exit(1);
	}

	return shader_program;
}

static GLuint generate_vao(GLfloat vertices[], GLfloat colors[]) {
	GLuint vao_id;
	glGenVertexArrays(1, &vao_id);
	glBindVertexArray(vao_id);

	GLuint vbo_id;
	glGenBuffers(1, &vbo_id);
	glBindBuffer(GL_ARRAY_BUFFER, vbo_id);

	GLuint num_vertices = 3;
	glBufferData(GL_ARRAY_BUFFER, num_vertices * 7 * sizeof (GLfloat), NULL, GL_STATIC_DRAW);
	glBufferSubData(GL_ARRAY_BUFFER, num_vertices * 0 * sizeof (GLfloat), num_vertices * 3 * sizeof (GLfloat), vertices);
	glBufferSubData(GL_ARRAY_BUFFER, num_vertices * 3 * sizeof (GLfloat), num_vertices * 4 * sizeof (GLfloat), colors);

	enum { POSITION, COLOR };
	glEnableVertexAttribArray(POSITION);
    glVertexAttribPointer(POSITION, 3, GL_FLOAT, GL_FALSE, 0, (void *) (num_vertices * 0 * sizeof (GLfloat)));
	glEnableVertexAttribArray(COLOR);
	glVertexAttribPointer(COLOR, 4, GL_FLOAT, GL_FALSE, 0, (void *) (num_vertices * 3 * sizeof (GLfloat)));

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	return vao_id;
}

static void init() {
	GLfloat right_vertices[] = {
		 0.0f, -0.5f,  0.0f,
		 1.0f, -0.5f,  0.0f,
		 0.5f,  0.5f,  0.0f
	};
	GLfloat left_vertices[] = {
		-1.0f, -0.5f, 0.0f,
		 0.0f, -0.5f, 0.0f,
		-0.5f,  0.5f, 0.0f
	};
	GLfloat colors[] = {
		0.0f, 1.0f, 0.0f, 1.0f,
		1.0f, 0.0f, 0.0f, 1.0f,
		0.0f, 0.0f, 1.0f, 1.0f
	};

	g_right_program_id = compile_shader(g_vert_shader, g_right_frag_shader);
	g_left_program_id = compile_shader(g_vert_shader, g_left_frag_shader);

	g_right_vao_id = generate_vao(right_vertices, colors);
	g_left_vao_id = generate_vao(left_vertices, colors);
}

static void display() {
	glClear(GL_COLOR_BUFFER_BIT);

	glUseProgram(g_right_program_id);
	glBindVertexArray(g_right_vao_id);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	glUseProgram(g_left_program_id);
	glBindVertexArray(g_left_vao_id);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	glUseProgram(0);
	glBindVertexArray(0);

    glutSwapBuffers();
}

int main(int argc, char **argv) {
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
	glutInitWindowSize(800, 600);
	glutCreateWindow("Hello Triangle");
	glutDisplayFunc(display);

	GLenum res = glewInit();
	if (res != GLEW_OK) {
		fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
		return 1;
	}

	init();
	glutMainLoop();
	return 0;
}
