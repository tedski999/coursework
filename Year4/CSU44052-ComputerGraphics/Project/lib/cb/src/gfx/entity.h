#ifndef CBI_GFX_ENTITY_H
#define CBI_GFX_ENTITY_H

#include <cglm/cglm.h>

struct cbi;

struct cb_entity *cbi_entity(struct cb_entity *parent);
void cbi_entity_preupdate(struct cb_entity *entity);
void cbi_entity_update(struct cb_entity *entity, struct cbi *cbi);
void cbi_entity_render(struct cb_entity *entity, mat4 *frame, mat4 *projection, double interpolation);
void cbi_entity_child(struct cb_entity *parent, struct cb_entity *child);
void cbi_entity_remove(struct cb_entity *entity);
void cbi_entity_delete(struct cb_entity *entity);

#endif
