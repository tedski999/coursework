#ifndef CBI_GFX_TRANSFORM_H
#define CBI_GFX_TRANSFORM_H

#include <cglm/cglm.h>

struct cb_transform;

void cbi_transform_update(struct cb_transform *transform);
void cbi_transform_lerp(struct cb_transform *transform, double interpolation, struct cb_transform *lerp);
void cbi_transform_matrix(struct cb_transform *transform, mat4 *matrix);
void cbi_transform_inverse_matrix(struct cb_transform *transform, mat4 *matrix);

#endif
