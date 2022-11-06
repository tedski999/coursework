#include "window.h"
#include "../cbi.h"
#include "../cfg.h"
#include "../util/alloc.h"
#include "../util/assert.h"
#include "../util/prof.h"
#include "../util/log.h"
#include <glad/gl.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

struct cbi_window {
	struct GLFWwindow *glfw;
};

static void on_key_input(struct GLFWwindow *glfw, int glfw_key, int scancode, int action, int mods) {
	cbi_prof(CBI_PROF_EVENT, "Key %d: %d", glfw_key, action);

	if (glfw_key < 0 || glfw_key > GLFW_KEY_LAST)
		return;
	if (action != GLFW_PRESS && action != GLFW_RELEASE)
		return;

	enum cb_key key;
	switch (glfw_key) {
		case GLFW_MOUSE_BUTTON_1: key = CB_MOUSE1; break; case GLFW_MOUSE_BUTTON_2: key = CB_MOUSE2; break; case GLFW_MOUSE_BUTTON_3: key = CB_MOUSE3; break; case GLFW_MOUSE_BUTTON_4: key = CB_MOUSE4; break;
		case GLFW_KEY_A:  key = CB_KEY_A;  break; case GLFW_KEY_B:  key = CB_KEY_B;  break; case GLFW_KEY_C:  key = CB_KEY_C;  break; case GLFW_KEY_D:  key = CB_KEY_D;  break; case GLFW_KEY_E:  key = CB_KEY_E;  break; case GLFW_KEY_F:   key = CB_KEY_F;   break; case GLFW_KEY_G:   key = CB_KEY_G;   break; case GLFW_KEY_H:   key = CB_KEY_H;   break;
		case GLFW_KEY_I:  key = CB_KEY_I;  break; case GLFW_KEY_J:  key = CB_KEY_J;  break; case GLFW_KEY_K:  key = CB_KEY_K;  break; case GLFW_KEY_L:  key = CB_KEY_L;  break; case GLFW_KEY_M:  key = CB_KEY_M;  break; case GLFW_KEY_N:   key = CB_KEY_N;   break; case GLFW_KEY_O:   key = CB_KEY_O;   break; case GLFW_KEY_P:   key = CB_KEY_P;   break;
		case GLFW_KEY_Q:  key = CB_KEY_Q;  break; case GLFW_KEY_R:  key = CB_KEY_R;  break; case GLFW_KEY_S:  key = CB_KEY_S;  break; case GLFW_KEY_T:  key = CB_KEY_T;  break; case GLFW_KEY_U:  key = CB_KEY_U;  break; case GLFW_KEY_V:   key = CB_KEY_V;   break; case GLFW_KEY_W:   key = CB_KEY_W;   break; case GLFW_KEY_X:   key = CB_KEY_X;   break;
		case GLFW_KEY_Y:  key = CB_KEY_Y;  break; case GLFW_KEY_Z:  key = CB_KEY_Z;  break; case GLFW_KEY_0:  key = CB_KEY_0;  break; case GLFW_KEY_1:  key = CB_KEY_1;  break; case GLFW_KEY_2:  key = CB_KEY_2;  break; case GLFW_KEY_3:   key = CB_KEY_3;   break; case GLFW_KEY_4:   key = CB_KEY_4;   break; case GLFW_KEY_5:   key = CB_KEY_5;   break;
		case GLFW_KEY_6:  key = CB_KEY_6;  break; case GLFW_KEY_7:  key = CB_KEY_7;  break; case GLFW_KEY_8:  key = CB_KEY_8;  break; case GLFW_KEY_9:  key = CB_KEY_9;  break; case GLFW_KEY_F1: key = CB_KEY_F1; break; case GLFW_KEY_F2:  key = CB_KEY_F2;  break; case GLFW_KEY_F3:  key = CB_KEY_F3;  break; case GLFW_KEY_F4:  key = CB_KEY_F4;  break;
		case GLFW_KEY_F5: key = CB_KEY_F5; break; case GLFW_KEY_F6: key = CB_KEY_F6; break; case GLFW_KEY_F7: key = CB_KEY_F7; break; case GLFW_KEY_F8: key = CB_KEY_F8; break; case GLFW_KEY_F9: key = CB_KEY_F9; break; case GLFW_KEY_F10: key = CB_KEY_F10; break; case GLFW_KEY_F11: key = CB_KEY_F11; break; case GLFW_KEY_F12: key = CB_KEY_F12; break;
		case GLFW_KEY_ESCAPE:       key = CB_KEY_ESCAPE;       break; case GLFW_KEY_GRAVE_ACCENT:  key = CB_KEY_GRAVE_ACCENT;  break; case GLFW_KEY_TAB:          key = CB_KEY_TAB;          break; case GLFW_KEY_CAPS_LOCK:     key = CB_KEY_CAPS_LOCK;     break;
		case GLFW_KEY_LEFT_SHIFT:   key = CB_KEY_SHIFT;        break; case GLFW_KEY_LEFT_CONTROL:  key = CB_KEY_CONTROL;       break; case GLFW_KEY_LEFT_SUPER:   key = CB_KEY_SUPER;        break; case GLFW_KEY_LEFT_ALT:      key = CB_KEY_ALT;           break;
		case GLFW_KEY_RIGHT_SHIFT:  key = CB_KEY_RIGHT_SHIFT;  break; case GLFW_KEY_RIGHT_CONTROL: key = CB_KEY_RIGHT_CONTROL; break; case GLFW_KEY_RIGHT_SUPER:  key = CB_KEY_RIGHT_SUPER;  break; case GLFW_KEY_RIGHT_ALT:     key = CB_KEY_RIGHT_ALT;     break;
		case GLFW_KEY_RIGHT:        key = CB_KEY_RIGHT;        break; case GLFW_KEY_LEFT:          key = CB_KEY_LEFT;          break; case GLFW_KEY_DOWN:         key = CB_KEY_DOWN;         break; case GLFW_KEY_UP:            key = CB_KEY_UP;            break;
		case GLFW_KEY_PAGE_UP:      key = CB_KEY_PAGE_UP;      break; case GLFW_KEY_PAGE_DOWN:     key = CB_KEY_PAGE_DOWN;     break; case GLFW_KEY_HOME:         key = CB_KEY_HOME;         break; case GLFW_KEY_END:           key = CB_KEY_END;           break;
		case GLFW_KEY_SLASH:        key = CB_KEY_SLASH;        break; case GLFW_KEY_BACKSLASH:     key = CB_KEY_BACKSLASH;     break; case GLFW_KEY_LEFT_BRACKET: key = CB_KEY_LEFT_BRACKET; break; case GLFW_KEY_RIGHT_BRACKET: key = CB_KEY_RIGHT_BRACKET; break;
		case GLFW_KEY_COMMA:        key = CB_KEY_COMMA;        break; case GLFW_KEY_PERIOD:        key = CB_KEY_PERIOD;        break; case GLFW_KEY_SEMICOLON:    key = CB_KEY_SEMICOLON;    break; case GLFW_KEY_APOSTROPHE:    key = CB_KEY_APOSTROPHE;    break;
		case GLFW_KEY_PRINT_SCREEN: key = CB_KEY_PRINT_SCREEN; break; case GLFW_KEY_SCROLL_LOCK:   key = CB_KEY_SCROLL_LOCK;   break; case GLFW_KEY_NUM_LOCK:     key = CB_KEY_NUM_LOCK;     break; case GLFW_KEY_PAUSE:         key = CB_KEY_PAUSE;         break;
		case GLFW_KEY_MINUS:        key = CB_KEY_MINUS;        break; case GLFW_KEY_EQUAL:         key = CB_KEY_EQUAL;         break; case GLFW_KEY_INSERT:       key = CB_KEY_INSERT;       break; case GLFW_KEY_DELETE:        key = CB_KEY_DELETE;        break;
		case GLFW_KEY_SPACE:        key = CB_KEY_SPACE;        break; case GLFW_KEY_ENTER:         key = CB_KEY_ENTER;         break; case GLFW_KEY_BACKSPACE:    key = CB_KEY_BACKSPACE;    break;
		default: key = CB_KEY_UNKNOWN; break;
	}

	struct cbi *cbi = glfwGetWindowUserPointer(glfw);
	cbi->cb.input.keys[key] = action | 0x2;
}

static void on_mouse_input(struct GLFWwindow *glfw, int button, int action, int mods) {
	on_key_input(glfw, button, 0, action, mods);
}

static void on_mouse_movement(struct GLFWwindow *glfw, double x, double y) {
	struct cbi *cbi = glfwGetWindowUserPointer(glfw);
	cbi->cb.input.mouse[vX] = x;
	cbi->cb.input.mouse[vY] = y;
}

static void on_scroll_input(struct GLFWwindow *glfw, double x, double y) {
	struct cbi *cbi = glfwGetWindowUserPointer(glfw);
	cbi->cb.input.scroll[vX] = x;
	cbi->cb.input.scroll[vY] = y;
}

static void on_framebuffer_resize(struct GLFWwindow *glfw, int w, int h) {
	cbi_prof(CBI_PROF_EVENT, "Window Resize: %dw %dh", w, h);
	struct cbi *cbi = glfwGetWindowUserPointer(glfw);
	int x = 0, y = 0;
#ifndef CB_STRETCH_ASPECT
	double xratio = CB_ASPECT * h / w;
	double yratio = 1.0 / xratio;
	xratio = fmin(xratio, 1.0);
	yratio = fmin(yratio, 1.0);
	x = w / 2.0 * (1.0 - xratio);
	y = h / 2.0 * (1.0 - yratio);
	w *= xratio;
	h *= yratio;
#endif
	glViewport(x, y, w, h);
	glfwSetCursorPos(glfw, cbi->cb.input.mouse[vX], cbi->cb.input.mouse[vY]);
	cbi_log(CB_LOG_INFO, "New screen region: %dx %dy %dw %dh", x, y, w, h);
}

static void on_window_close(struct GLFWwindow *glfw) {
	cbi_prof(CBI_PROF_EVENT, "Window Close");
	cbi_log(CB_LOG_INFO, "Window is closing!");
	struct cbi *cbi = glfwGetWindowUserPointer(glfw);
	cbi->cb.is_running = false;
}

#ifndef CB_NO_LOG

static void on_glfw_error(int code, const char *msg) {
	cbi_prof(CBI_PROF_EVENT, "GLFW Error %d: %s", code, msg);
	cbi_log(CB_LOG_WARN, "GLFW Error %d: %s", code, msg);
}

#ifdef GL_VERSION_4_3
static void on_opengl_msg(GLenum src, GLenum type, unsigned id, GLenum sev, GLsizei len, const GLchar *msg, const void *user) {
	cbi_prof(CBI_PROF_EVENT, "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
	cbi_log(CB_LOG_DBUG, "OpenGL (type = 0x%x, severity = 0x%x): %s", type, sev, msg);
}
#endif

#endif

struct cbi_window *cbi_window_open(struct cbi *cbi) {
	cbi_log(CB_LOG_DBUG, "Opening window...");
	struct cbi_window *window = cbi_malloc(sizeof *window);

#ifndef CB_NO_LOG
	glfwSetErrorCallback(on_glfw_error);
#endif

	cbi_log(CB_LOG_INFO, "GLFW %s", glfwGetVersionString());
	cbi_assert(glfwInit(), "Failed to initialse GLFW");

	GLFWmonitor *monitor = glfwGetPrimaryMonitor();
	cbi_assert(monitor, "Unable to detect device monitor");
	const GLFWvidmode *mode = glfwGetVideoMode(monitor);
	cbi_assert(monitor, "Unable to get device video mode");

	glfwWindowHint(GLFW_RED_BITS, mode->redBits);
	glfwWindowHint(GLFW_GREEN_BITS, mode->greenBits);
	glfwWindowHint(GLFW_BLUE_BITS, mode->blueBits);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_SAMPLES, CB_MULTISAMPLES);
	glfwWindowHint(GLFW_CENTER_CURSOR, GLFW_TRUE);
	// window->glfw = glfwCreateWindow(mode->width, mode->height, "", monitor, NULL);
	window->glfw = glfwCreateWindow(mode->width, mode->height, "", NULL, NULL);
	cbi_assert(window->glfw, "Unable to open window");

	glfwSetInputMode(window->glfw, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
	if (glfwRawMouseMotionSupported())
		glfwSetInputMode(window->glfw, GLFW_RAW_MOUSE_MOTION, GL_TRUE);

	glfwMakeContextCurrent(window->glfw);
	glfwSetWindowUserPointer(window->glfw, cbi);
	glfwSetKeyCallback(window->glfw, on_key_input);
	glfwSetMouseButtonCallback(window->glfw, on_mouse_input);
	glfwSetCursorPosCallback(window->glfw, on_mouse_movement);
	glfwSetScrollCallback(window->glfw, on_scroll_input);
	glfwSetFramebufferSizeCallback(window->glfw, on_framebuffer_resize);
	glfwSetWindowCloseCallback(window->glfw, on_window_close);

	cbi_log(CB_LOG_DBUG, "Loading OpenGL...");
	int version = gladLoadGL(glfwGetProcAddress);
	cbi_assert(version, "Failed to load OpenGL");

	cbi_log(CB_LOG_DBUG, "Loaded OpenGL %d.%d", GLAD_VERSION_MAJOR(version), GLAD_VERSION_MINOR(version));
	cbi_log(CB_LOG_INFO, "OpenGL %s", glGetString(GL_VERSION));

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_MULTISAMPLE);

#ifndef CB_NO_LOG
#ifdef GL_VERSION_4_3
	if (GLAD_VERSION_MAJOR(version) >= 4 && GLAD_VERSION_MINOR(version) >= 3) {
		glEnable(GL_DEBUG_OUTPUT);
		glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, 0, GL_TRUE);
		glDebugMessageCallback(on_opengl_msg, 0);
	}
#endif
#endif
	glfwSwapInterval(0);

	glfwPollEvents();
	return window;
}

void cbi_window_draw(struct cbi_window *window) {
	cbi_prof(CBI_PROF_START, "Drawing Window");
	glfwSwapBuffers(window->glfw);
	cbi_prof(CBI_PROF_END, "Drawing Window");
}

void cbi_window_title(struct cbi_window *window, char *title) {
	glfwSetWindowTitle(window->glfw, title);
}

void cbi_window_close(struct cbi_window *window) {
	cbi_log(CB_LOG_DBUG, "Closing window...");
	glfwDestroyWindow(window->glfw);
	glfwTerminate();
	cbi_free(window);
}
