#include "prof.h"
#include "alloc.h"
#include "log.h"
#include "assert.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <libgen.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <time.h>

#undef cbi_prof

#ifndef CB_NO_PROF

#define USEC 1000000
#define INITIAL_EVENTS_CAP 1024

struct event {
	char *name;
	int line;
	const char *func;
	const char *file;
	enum cbi_prof_event type;
	double time;
};

static bool is_profiling;
static char filename[sizeof "cb000000000000.json"];
static struct event *events;
static int events_len, events_cap;
static const char *phases[cbi_prof_event_len] = { "i", "B", "E" };

void cbi_prof_start(void) {
	cbi_assert(!is_profiling, "Profiler already started");
	cbi_log(CB_LOG_DBUG, "Starting profiler...");
    time_t t = time(NULL);
    strftime(filename, sizeof filename, "cb%y%m%d%H%M%S.json", gmtime(&t));
	events_len = 0;
	events_cap = INITIAL_EVENTS_CAP;
	events = cbi_malloc(sizeof *events * events_cap);
	is_profiling = true;
}

void cbi_prof(int line, const char *func, const char *file, enum cbi_prof_event event, ...) {
	if (!is_profiling)
		return;

	char *name = NULL;
	va_list args; va_start(args, event);
	char *format = va_arg(args, char *);
	if (format) {
		va_list args_copy; va_copy(args_copy, args);
		int name_len = vsnprintf(NULL, 0, format, args_copy) + 1;
		name = cbi_malloc(sizeof *name * name_len);
		vsnprintf(name, name_len, format, args);
		va_end(args_copy);
	}
	va_end(args);

	const char *escaped_file = strrchr(file, '\\');
	if (escaped_file)
		file = escaped_file + 1;

	if (++events_len > events_cap)
		events = cbi_realloc(events, sizeof *events * (events_cap *= 2));
	events[events_len - 1] = (struct event) { name, line, func, file, event, glfwGetTime() };
}

void cbi_prof_finish(void) {
	if (!is_profiling)
		return;
	is_profiling = false;

	cbi_log(CB_LOG_DBUG, "Writing profiling data to %s...", filename);
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
		cbi_free(events[i].name);
	}
	fprintf(f, "]}\n");
	fclose(f);
	cbi_free(events);
}

void cbi_prof_toggle(void) {
	if (is_profiling) {
		cbi_prof_finish();
	} else {
		cbi_prof_start();
	}
}

#endif
