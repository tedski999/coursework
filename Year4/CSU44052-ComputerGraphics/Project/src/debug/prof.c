#include "prof.h"
#include "log.h"
#include "../platform/alloc.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <libgen.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <time.h>

#undef cb_prof

#ifndef CB_NO_PROF

#define USEC 1000000

struct event {
	char *name;
	int line;
	const char *func;
	const char *file;
	enum cb_prof_event type;
	double time;
};

static struct event *events;
static int events_len, events_cap = 1024;
static const char *phases[cb_prof_event_len] = { "i", "B", "E" };

void cb_prof(int line, const char *func, const char *file, enum cb_prof_event event, ...) {
	va_list ap; va_start(ap, event);
	char *name = NULL, *format = va_arg(ap, char *);
	if (format) {
		va_list ap_copy; va_copy(ap_copy, ap);
		int name_len = vsnprintf(NULL, 0, format, ap_copy) + 1;
		name = cb_malloc(sizeof *name * name_len);
		vsnprintf(name, name_len, format, ap);
		va_end(ap_copy);
	}
	va_end(ap);
	const char *escaped_file = strrchr(file, '\\');
	if (escaped_file)
		file = escaped_file + 1;
	if (++events_len > events_cap || !events)
		events = cb_realloc(events, sizeof *events * (events_cap *= 2));
	events[events_len - 1] = (struct event) { name, line, func, file, event, glfwGetTime() };
}

void cb_prof_write(void) {
    time_t t = time(NULL);
	char filename[sizeof "cb000000000000.json"];
    strftime(filename, sizeof filename, "cb%y%m%d%H%M%S.json", gmtime(&t));
	cb_log(CB_LOG_DBUG, "Writing profiling data to %s...", filename);
	FILE *f = fopen(filename, "w");
	fprintf(f, "{\"traceEvents\":[");
	for (int i = 0; i < events_len; i++) {
		if (i != 0) fprintf(f, ",");
		fprintf(f, "{");
		fprintf(f,   "\"cat\":\"%s\",", events[i].file);
		fprintf(f,   "\"name\":\"%s\",", events[i].name ? events[i].name : events[i].func);
		fprintf(f,   "\"ph\":\"%s\",", phases[events[i].type]);
		fprintf(f,   "\"ts\":\"%f\",", events[i].time * USEC);
		fprintf(f,   "\"pid\":0,");
		fprintf(f,   "\"tid\":0,");
		fprintf(f,   "\"args\":{");
		fprintf(f,     "\"line\":\"%d\",", events[i].line);
		fprintf(f,     "\"function\":\"%s\"", events[i].func);
		fprintf(f,   "}");
		fprintf(f, "}");
	}
	fprintf(f, "]}\n");
	fclose(f);
}

#endif
