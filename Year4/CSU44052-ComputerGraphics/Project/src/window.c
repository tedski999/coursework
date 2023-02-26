#include "window.h"
#include "assert.h"
#include "config.h"
#include "log.h"
#include "prof.h"
#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <cglm/cglm.h>

extern GLFWwindow *window;
extern int keys[GLFW_KEY_LAST];
extern vec2 mouse, mouse_diff, scroll;
extern bool is_running;

static void on_key_input(GLFWwindow *win, int key, int scancode, int action, int mods) {
	if (action != GLFW_REPEAT) keys[key] = (action == GLFW_PRESS);
}

static void on_mouse_input(GLFWwindow *win, int button, int action, int mods) {
	on_key_input(win, button, 0, action, mods);
}

static void on_mouse_movement(GLFWwindow *win, double x, double y) {
	glm_vec2_copy((vec2){x,y}, mouse);
}

static void on_scroll_input(GLFWwindow *window, double x, double y) {
	glm_vec2_copy((vec2){x,y}, scroll);
}

static void on_window_close(GLFWwindow *win) {
	is_running = false;
}

static void on_framebuffer_resize(GLFWwindow *win, int w, int h) {
	double r = WW_ASPECT * h / w, x = fmin(r, 1), y = fmin(1 / r, 1);
	glViewport(w / 2.0 * (1 - x), h / 2.0 * (1 - y), w * x, h * y);
	glfwSetCursorPos(win, mouse[0], mouse[1]);
}

static void on_glfw_error(int code, const char *msg) {
	ww_prof(WW_PROF_EVENT, "GLFW Error %d: %s", code, msg);
	ww_log(WW_LOG_WARN,    "GLFW Error %d: %s", code, msg);
}

#if !defined(WW_NO_LOG) && defined(GL_VERSION_4_3)
static void on_gl_msg(GLenum src, GLenum type, unsigned id, GLenum sev, GLsizei len, const GLchar *msg, const void *user) {
	ww_prof(WW_PROF_EVENT, "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
	ww_log(WW_LOG_DBUG,    "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
}
#endif

void ww_window_open(char *title) {
	ww_log(WW_LOG_DBUG, "Opening window...");

	ww_log(WW_LOG_INFO, "GLFW %s", glfwGetVersionString());
	ww_assert(glfwInit(), "Failed to initialse GLFW");
	glfwSetErrorCallback(on_glfw_error);

	GLFWmonitor *monitor = glfwGetPrimaryMonitor();
	ww_assert(monitor, "Unable to detect device monitor");

	const GLFWvidmode *mode = glfwGetVideoMode(monitor);
	ww_assert(monitor, "Unable to get device video mode");

	glfwWindowHint(GLFW_SAMPLES, WW_MULTISAMPLES);
	window = glfwCreateWindow(mode->width, mode->height, "Winter Wonderland", monitor, NULL);
	ww_assert(window, "Unable to open window");

	glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
	if (glfwRawMouseMotionSupported())
		glfwSetInputMode(window, GLFW_RAW_MOUSE_MOTION, GLFW_TRUE);

	glfwMakeContextCurrent(window);
	glfwSetKeyCallback(window, on_key_input);
	glfwSetMouseButtonCallback(window, on_mouse_input);
	glfwSetCursorPosCallback(window, on_mouse_movement);
	glfwSetScrollCallback(window, on_scroll_input);
	glfwSetFramebufferSizeCallback(window, on_framebuffer_resize);
	glfwSetWindowCloseCallback(window, on_window_close);

	ww_log(WW_LOG_DBUG, "Initialising OpenGL...");
	int version = gladLoadGL(glfwGetProcAddress);
	ww_assert(version, "Failed to load OpenGL");
	ww_log(WW_LOG_INFO, "OpenGL %s", glGetString(GL_VERSION));

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_MULTISAMPLE);
    glEnable(GL_CULL_FACE);
    glFrontFace(GL_CCW);
    glCullFace(GL_BACK);

#if !defined(WW_NO_LOG) && defined(GL_VERSION_4_3)
	if (GLAD_VERSION_MAJOR(version) >= 4 && GLAD_VERSION_MINOR(version) >= 3) {
		glEnable(GL_DEBUG_OUTPUT);
		glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, 0, GL_TRUE);
		glDebugMessageCallback(on_gl_msg, 0);
	}
#endif

	glfwPollEvents();
}

void ww_window_close(void) {
	glfwDestroyWindow(window);
	glfwTerminate();
}
