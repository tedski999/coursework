#ifndef CB_H
#define CB_H

#include <stdbool.h>
#include <cglm/cglm.h>

enum cb_vector_component {
	vX, vY, vZ, vW
};

enum cb_key_state {
	CB_IS_KEY_UP, CB_IS_KEY_DOWN,
	CB_ON_KEY_UP, CB_ON_KEY_DOWN
};

enum cb_key {
	CB_MOUSE1, CB_MOUSE2, CB_MOUSE3, CB_MOUSE4,
	CB_KEY_A,  CB_KEY_B,  CB_KEY_C,  CB_KEY_D,  CB_KEY_E,  CB_KEY_F,   CB_KEY_G,   CB_KEY_H,
	CB_KEY_I,  CB_KEY_J,  CB_KEY_K,  CB_KEY_L,  CB_KEY_M,  CB_KEY_N,   CB_KEY_O,   CB_KEY_P,
	CB_KEY_Q,  CB_KEY_R,  CB_KEY_S,  CB_KEY_T,  CB_KEY_U,  CB_KEY_V,   CB_KEY_W,   CB_KEY_X,
	CB_KEY_Y,  CB_KEY_Z,  CB_KEY_0,  CB_KEY_1,  CB_KEY_2,  CB_KEY_3,   CB_KEY_4,   CB_KEY_5,
	CB_KEY_6,  CB_KEY_7,  CB_KEY_8,  CB_KEY_9,  CB_KEY_F1, CB_KEY_F2,  CB_KEY_F3,  CB_KEY_F4,
	CB_KEY_F5, CB_KEY_F6, CB_KEY_F7, CB_KEY_F8, CB_KEY_F9, CB_KEY_F10, CB_KEY_F11, CB_KEY_F12,
	CB_KEY_ESCAPE,       CB_KEY_GRAVE_ACCENT,  CB_KEY_TAB,          CB_KEY_CAPS_LOCK,
	CB_KEY_SHIFT,        CB_KEY_CONTROL,       CB_KEY_SUPER,        CB_KEY_ALT,
	CB_KEY_RIGHT_SHIFT,  CB_KEY_RIGHT_CONTROL, CB_KEY_RIGHT_SUPER,  CB_KEY_RIGHT_ALT,
	CB_KEY_RIGHT,        CB_KEY_LEFT,          CB_KEY_DOWN,         CB_KEY_UP,
	CB_KEY_PAGE_UP,      CB_KEY_PAGE_DOWN,     CB_KEY_HOME,         CB_KEY_END,
	CB_KEY_SLASH,        CB_KEY_BACKSLASH,     CB_KEY_LEFT_BRACKET, CB_KEY_RIGHT_BRACKET,
	CB_KEY_COMMA,        CB_KEY_PERIOD,        CB_KEY_SEMICOLON,    CB_KEY_APOSTROPHE,
	CB_KEY_PRINT_SCREEN, CB_KEY_SCROLL_LOCK,   CB_KEY_NUM_LOCK,     CB_KEY_PAUSE,
	CB_KEY_MINUS,        CB_KEY_EQUAL,         CB_KEY_INSERT,       CB_KEY_DELETE,
	CB_KEY_SPACE,        CB_KEY_ENTER,         CB_KEY_BACKSPACE,    CB_KEY_UNKNOWN,
	CB_KEY_LAST
};

struct cb;
struct cb_entity;
typedef void cb_update_fn(struct cb_entity *player, struct cb *cb);
typedef void cb_stop_fn(struct cb_entity *player);

struct cb_transform {
	vec3 position, rotation, scale;
	vec3 last_position, last_rotation, last_scale;
};

struct cb_entity {
	struct cb_transform transform;
	struct cb_model *model;
	struct cb_texture *texture;
	struct cb_shader *shader;
	struct cb_entity *parent;
	struct cb_entity **children;
	int children_len;
	cb_update_fn *update_fn;
	cb_stop_fn *stop_fn;
	void *data;
	int id;
};

struct cb_input {
	enum cb_key_state keys[CB_KEY_LAST];
	vec2 mouse, dmouse;
	vec2 scroll;
};

struct cb_camera {
	struct cb_transform transform;
	double fov;
};

struct cb {
	bool is_running;
	struct cb_entity scene;
	struct cb_camera camera;
	struct cb_input input;
	// struct cb_texture *skybox[6];
};

struct cb_entity *cb_entity(struct cb_entity *parent);
void cb_entity_child(struct cb_entity *parent, struct cb_entity *child);
double cb_entity_distance(struct cb_entity *a, struct cb_entity *b);
void cb_entity_delete(struct cb_entity *entity);

struct cb_model *cb_model(struct cb *cb, char *name);
struct cb_texture *cb_texture(struct cb *cb, char *name);
struct cb_shader *cb_shader(struct cb *cb, char *vertname, char *fragname);
void cb_model_delete(struct cb_model *model);
void cb_texture_delete(struct cb_texture *texture);
void cb_shader_delete(struct cb_shader *shader);

void cb_window_title(struct cb *cb, char *title);

extern void cb_main(struct cb *cb);

#endif
