#ifndef CB_CORE_H
#define CB_CORE_H

#include "scene/entity.h"
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <cglm/cglm.h>

#ifndef CB_ASPECT
#define CB_ASPECT (16.0 / 9.0)
#endif

#ifndef CB_MULTISAMPLES
#define CB_MULTISAMPLES 16
#endif

#ifndef CB_WAD
#define CB_WAD "/data.wad"
#endif

#ifndef CB_TPS
#define CB_TPS 60
#endif

#ifndef CB_MAX_DT
#define CB_MAX_DT 1.0
#endif

#ifndef CB_LOG_VERBOSITY
#define CB_LOG_VERBOSITY CB_LOG_INFO
#endif

struct cb {
	bool is_running;
	struct cb_entity scene;
	struct GLFWwindow *window;
	struct { bool keys[GLFW_KEY_LAST]; vec2 mouse; vec2 scroll; } input;
	struct zip *wad;
};

#endif
