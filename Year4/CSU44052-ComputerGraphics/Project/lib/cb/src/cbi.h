#ifndef CBI_CBI_H
#define CBI_CBI_H

#include <cb.h>
#include "core/wad.h"
#include "core/window.h"

struct cbi {
	struct cb cb;
	struct cbi_wad *wad;
	struct cbi_window *window;
};

#endif
