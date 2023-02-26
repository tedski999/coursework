#ifndef WW_APPLICATION_GROUND_H
#define WW_APPLICATION_GROUND_H

#include <glad/gl.h>
#include <cglm/cglm.h>

struct ww_entity *ww_ground(int seed, int detail, double frequency, double scale, double size, GLuint shader);
double ww_ground_get_height(struct ww_entity *ground, double x, double y);
void ww_ground_deform(struct ww_entity *ground, double x, double z, double s);

#endif
