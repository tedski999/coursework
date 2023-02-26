#include "prof.h"
#include "alloc.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#undef ww_prof

#ifndef WW_NO_PROF

static bool first = true;
static char *phases[ww_prof_event_len] = { "i", "B", "E" };

void ww_prof(int line, const char *func, const char *file, enum ww_prof_event event, ...) {
	va_list ap;
	va_start(ap, event);
	const char *format = va_arg(ap, char *);
	if (!format)
		format = file;
	char *escaped_file = strrchr(file, '\\');
	if (escaped_file)
		file = escaped_file + 1;
	fprintf(stderr, first ? "{\"traceEvents\":[" : ",");
	fprintf(stderr, "{");
	fprintf(stderr,   "\"cat\":\"%s\",", file);
	fprintf(stderr,   "\"name\":\""); vfprintf(stderr, format, ap); fprintf(stderr, "\",");
	fprintf(stderr,   "\"ph\":\"%s\",", phases[event]);
	fprintf(stderr,   "\"ts\":\"%f\",", glfwGetTime() * 1000000);
	fprintf(stderr,   "\"pid\":0,");
	fprintf(stderr,   "\"tid\":0,");
	fprintf(stderr,   "\"args\":{");
	fprintf(stderr,     "\"line\":\"%d\",", line);
	fprintf(stderr,     "\"function\":\"%s\"", func);
	fprintf(stderr,   "}");
	fprintf(stderr, "}");
	va_end(ap);
	first = false;
}

#endif
