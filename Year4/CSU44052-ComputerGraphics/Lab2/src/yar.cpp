#include "yar.hpp"
#include "model.hpp"
#include "shader.hpp"
#include "object.hpp"
#include <iostream>
#include <glm/glm.hpp>
#include <glm/ext/matrix_clip_space.hpp>
#include <glm/ext/matrix_transform.hpp>

#define ASPECT_RATIO (16.0f / 9.0f)
#define TPS 60
#define MAX_DT 0.25

yar::yar::yar() {
	glfwInit();
	GLFWmonitor *monitor = glfwGetPrimaryMonitor();
	const GLFWvidmode *mode = glfwGetVideoMode(monitor);

	glfwWindowHint(GLFW_RED_BITS, mode->redBits);
	glfwWindowHint(GLFW_GREEN_BITS, mode->greenBits);
	glfwWindowHint(GLFW_BLUE_BITS, mode->blueBits);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_CENTER_CURSOR, GLFW_FALSE);
	window = glfwCreateWindow(mode->width, mode->height, "Yet Another Renderer", monitor, NULL);
	glfwMakeContextCurrent(window);
	glfwSetFramebufferSizeCallback(window, [](GLFWwindow *window, int w, int h) {
		double xratio = ASPECT_RATIO * h / w;
		double yratio = 1.0 / xratio;
		xratio = fmin(xratio, 1.0);
		yratio = fmin(yratio, 1.0);
		glViewport(
			w / 2.0 * (1.0 - xratio),
			h / 2.0 * (1.0 - yratio),
			w * xratio, h * yratio);
	});

	GLenum res = glewInit();
	if (res != GLEW_OK) {
		std::cerr << "Error: "<< glewGetErrorString(res) << std::endl;
		exit(1);
	}

	glEnable(GL_DEPTH_TEST);

	teapot_model = new model("./res/models/teapot.stl");
	teacup_model = new model("./res/models/teacup.blend");

	basic_shader = new shader("./res/shaders/basic.vert.glsl", "./res/shaders/basic.frag.glsl");
	simple_lighting_shader = new shader("./res/shaders/simple_lighting.vert.glsl", "./res/shaders/basic.frag.glsl");
	rainbow_lighting_shader = new shader("./res/shaders/rainbow_lighting.vert.glsl", "./res/shaders/basic.frag.glsl");

	teapot =  new object(teapot_model, rainbow_lighting_shader, glm::vec3( 0.0f,    0.0f,   0.0f), glm::vec3(  0.0f,   0.0f,   0.0f), glm::vec3(1.0f, 1.0f, 1.0f));
	teacup1 = new object(teacup_model, simple_lighting_shader,  glm::vec3( 15.0f,  20.0f,   5.0f), glm::vec3( 90.0f, 180.0f,   0.0f), glm::vec3(1.0f, 1.0f, 1.0f));
	teacup2 = new object(teacup_model, simple_lighting_shader,  glm::vec3(-10.0f,   0.0f, -10.0f), glm::vec3(  0.0f,   0.0f,  90.0f), glm::vec3(2.0f, 0.5f, 0.5f));
	teacup3 = new object(teacup_model, simple_lighting_shader,  glm::vec3( 10.0f, -15.0f,   0.0f), glm::vec3( 90.0f,   0.0f,   0.0f), glm::vec3(1.0f, 1.0f, 1.0f));

	glm::mat4 projection = glm::perspective(glm::radians(45.0f), ASPECT_RATIO, 0.1f, 1000.0f);
	glm::mat4 view = glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.0f, -50.0f));

	basic_shader->set_uniform("view", view);
	basic_shader->set_uniform("projection", projection);
	simple_lighting_shader->set_uniform("view", view);
	simple_lighting_shader->set_uniform("projection", projection);
	rainbow_lighting_shader->set_uniform("view", view);
	rainbow_lighting_shader->set_uniform("projection", projection);

	is_running = true;
}

yar::yar::~yar() {
	delete teapot_model;
	delete teacup_model;
	delete basic_shader;
	delete simple_lighting_shader;
	delete rainbow_lighting_shader;
	delete teapot;
	delete teacup1;
	delete teacup2;
	delete teacup3;
	glfwTerminate();
}

void yar::yar::run() {
	double time = 1.0 / TPS; // update before first render
	while (!glfwWindowShouldClose(window)) {
		time += fmin(glfwGetTime(), MAX_DT);
		while (time >= 1.0 / TPS) {
			time -= 1.0 / TPS;
			update();
		}
		render();
	}
}

void yar::yar::update() {
	glfwPollEvents();

	double x, y;
	glfwGetCursorPos(window, &x, &y);

	glm::mat4 view = glm::mat4(1.0f);
	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -50.0f));
	view = glm::rotate(view, glm::radians((float) y), glm::vec3(1.0f, 0.0f, 0.0f));
	view = glm::rotate(view, glm::radians((float) x), glm::vec3(0.0f, 0.0f, 1.0f));

	basic_shader->set_uniform("view", view);
	simple_lighting_shader->set_uniform("view", view);
	rainbow_lighting_shader->set_uniform("view", view);

	static float f = 0;
	f += 0.005f;

	teapot->position.z = sin(f);
	teacup1->rotation += 0.1f;
	teacup2->scale.x = sin(f) * 0.5f + 1.0f;
	teacup2->scale.y = cos(f) * 0.5f + 1.0f;
	teacup3->rotation.z += 0.1f;
}

void yar::yar::render() {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	teapot->render();
	teacup1->render();
	teacup2->render();
	teacup3->render();
	glfwSwapBuffers(window);
}
