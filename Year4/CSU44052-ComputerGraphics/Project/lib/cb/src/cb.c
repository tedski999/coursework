#include <cb.h>
#include "cbi.h"
#include "data/model.h"
#include "data/texture.h"
#include "data/shader.h"
#include "gfx/entity.h"
#include "util/assert.h"

#if !(defined __unix__ || defined linux || defined _linux || defined __linux__)
#error "Cardboard does not support target platform"
#endif

#ifdef CB_NO_LOG
#pragma message "Cardboard logging disabled"
#endif

#ifdef CB_NO_PROF
#pragma message "Cardboard profiling disabled"
#endif

#ifdef CB_NO_ASSERT_DETAIL
#pragma message "Cardboard assertion detail disabled"
#endif

struct cb_entity *cb_entity(struct cb_entity *parent) {
	cbi_assert(parent);
	return cbi_entity(parent);
}

void cb_entity_child(struct cb_entity *parent, struct cb_entity *child) {
	cbi_assert(parent && child);
	cbi_entity_child(parent, child);
}

void cb_entity_delete(struct cb_entity *entity) {
	cbi_assert(entity);
	cbi_entity_delete(entity);
}

struct cb_model *cb_model(struct cb *cb, char *name) {
	cbi_assert(cb && name);
	struct cbi *cbi = (struct cbi *) cb;
	return cbi_model(cbi->wad, name);
}

struct cb_texture *cb_texture(struct cb *cb, char *name) {
	cbi_assert(cb && name);
	struct cbi *cbi = (struct cbi *) cb;
	return cbi_texture(cbi->wad, name);
}

struct cb_shader *cb_shader(struct cb *cb, char *vert_name, char *frag_name) {
	cbi_assert(cb && vert_name && frag_name);
	struct cbi *cbi = (struct cbi *) cb;
	return cbi_shader(cbi->wad, vert_name, frag_name);
}

void cb_model_delete(struct cb_model *model) {
	cbi_assert(model);
	// TODO
}

void cb_texture_delete(struct cb_texture *texture) {
	cbi_assert(texture);
	// TODO
}

void cb_shader_delete(struct cb_shader *shader) {
	cbi_assert(shader);
	// TODO
}

void cb_window_title(struct cb *cb, char *title) {
	cbi_assert(cb && title);
	struct cbi *cbi = (struct cbi *) cb;
	cbi_window_title(cbi->window, title);
}
