#include "window.h"
#include "../debug/assert.h"
#include "../debug/log.h"
#include "../debug/prof.h"
#include "../core.h"
#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <cglm/cglm.h>

static void on_key_input(struct GLFWwindow *window, int key, int scancode, int action, int mods) {
	cb_prof(CB_PROF_EVENT, "Key %d: %d", key, action);
	if (action == GLFW_PRESS || action == GLFW_RELEASE) {
		struct cb *cb = glfwGetWindowUserPointer(window);
		cb->input.keys[key] = (action == GLFW_PRESS);
	}
}

static void on_mouse_input(struct GLFWwindow *window, int button, int action, int mods) {
	on_key_input(window, button, 0, action, mods);
}

static void on_mouse_movement(struct GLFWwindow *window, double x, double y) {
	struct cb *cb = glfwGetWindowUserPointer(window);
	glm_vec2_copy((vec2) {x,y}, cb->input.mouse);
}

static void on_scroll_input(struct GLFWwindow *window, double x, double y) {
	struct cb *cb = glfwGetWindowUserPointer(window);
	glm_vec2_copy((vec2) {x,y}, cb->input.scroll);
}

static void on_framebuffer_resize(struct GLFWwindow *glfw, int w, int h) {
	cb_prof(CB_PROF_EVENT, "Window Resize: %dw %dh", w, h);
	cb_log(CB_LOG_DBUG, "Window Resize: %dw %dh", w, h);
	double xratio = fmin(      (CB_ASPECT*h/w), 1.0);
	double yratio = fmin(1.0 / (CB_ASPECT*h/w), 1.0);
	glViewport(w / 2.0 * (1.0 - xratio), h / 2.0 * (1.0 - yratio), w * xratio, h * yratio);
	struct cb *cb = glfwGetWindowUserPointer(glfw);
	glfwSetCursorPos(glfw, cb->input.mouse[0], cb->input.mouse[1]);
}

static void on_window_close(struct GLFWwindow *glfw) {
	cb_prof(CB_PROF_EVENT, "Window Close");
	struct cb *cb = glfwGetWindowUserPointer(glfw);
	cb->is_running = false;
}

#ifndef CB_NO_LOG
static void on_glfw_error(int code, const char *msg) {
	cb_prof(CB_PROF_EVENT, "GLFW Error %d: %s", code, msg);
	cb_log(CB_LOG_WARN, "GLFW Error %d: %s", code, msg);
}
#ifdef GL_VERSION_4_3
static void on_gl_msg(GLenum src, GLenum type, unsigned id, GLenum sev, GLsizei len, const GLchar *msg, const void *user) {
	cb_prof(CB_PROF_EVENT, "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
	cb_log(CB_LOG_DBUG, "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
}
#endif
#endif

struct GLFWwindow *cb_window_open(struct cb *cb) {
	cb_log(CB_LOG_DBUG, "Opening window...");

#ifndef CB_NO_LOG
	glfwSetErrorCallback(on_glfw_error);
#endif

	cb_log(CB_LOG_INFO, "GLFW %s", glfwGetVersionString());
	cb_assert(glfwInit(), "Failed to initialse GLFW");
	GLFWmonitor *monitor = glfwGetPrimaryMonitor();
	cb_assert(monitor, "Unable to detect device monitor");
	const GLFWvidmode *mode = glfwGetVideoMode(monitor);
	cb_assert(monitor, "Unable to get device video mode");

	glfwWindowHint(GLFW_RED_BITS, mode->redBits);
	glfwWindowHint(GLFW_GREEN_BITS, mode->greenBits);
	glfwWindowHint(GLFW_BLUE_BITS, mode->blueBits);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_SAMPLES, CB_MULTISAMPLES);
	glfwWindowHint(GLFW_CENTER_CURSOR, GLFW_TRUE);

	GLFWwindow *window = glfwCreateWindow(mode->width, mode->height, "Winter Wonderland", monitor, NULL);
	cb_assert(window, "Unable to open window");
	glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
	if (glfwRawMouseMotionSupported())
		glfwSetInputMode(window, GLFW_RAW_MOUSE_MOTION, GLFW_TRUE);

	glfwMakeContextCurrent(window);
	glfwSetWindowUserPointer(window, cb);
	glfwSetKeyCallback(window, on_key_input);
	glfwSetMouseButtonCallback(window, on_mouse_input);
	glfwSetCursorPosCallback(window, on_mouse_movement);
	glfwSetScrollCallback(window, on_scroll_input);
	glfwSetFramebufferSizeCallback(window, on_framebuffer_resize);
	glfwSetWindowCloseCallback(window, on_window_close);

	cb_log(CB_LOG_DBUG, "Loading OpenGL...");

	int version = gladLoadGL(glfwGetProcAddress);
	cb_assert(version, "Failed to load OpenGL");
	cb_log(CB_LOG_INFO, "OpenGL %s", glGetString(GL_VERSION));
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_MULTISAMPLE);

#ifndef CB_NO_LOG
#ifdef GL_VERSION_4_3
	if (GLAD_VERSION_MAJOR(version) >= 4 && GLAD_VERSION_MINOR(version) >= 3) {
		glEnable(GL_DEBUG_OUTPUT);
		glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, 0, GL_TRUE);
		glDebugMessageCallback(on_gl_msg, 0);
	}
#endif
#endif

	return window;
}

void cb_window_close(struct GLFWwindow *window) {
	cb_log(CB_LOG_DBUG, "Closing window...");
	glfwDestroyWindow(window);
	glfwTerminate();
}
