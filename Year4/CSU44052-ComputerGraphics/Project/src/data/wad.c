#include "wad.h"
#include "../core.h"
#include "../platform/alloc.h"
#include "../debug/assert.h"
#include "../debug/prof.h"
#include "../debug/log.h"
#include <string.h>
#include <libgen.h>
#include <zip.h>

struct zip *cb_wad_open(char *exe_path) {
	cb_log(CB_LOG_DBUG, "Opening WAD...");

	int err;
	char *path = dirname(strdup(exe_path));
	path = strcat(cb_realloc(path, strlen(path) + strlen(CB_WAD) + 1), CB_WAD);
	struct zip *wad = zip_open(path, ZIP_RDONLY, &err);
	cb_assert(wad);

	cb_log(CB_LOG_INFO, "Using WAD %s", path);
	cb_free(path);
	return wad;
}

char *cb_wad_load(struct zip *wad, char *path, int *len) {
	cb_prof(CB_PROF_START, "WAD Load %s", path);
	cb_log(CB_LOG_DBUG, "Loading %s from WAD...", path);

	struct zip_stat stat;
	struct zip_file *fd = zip_fopen(wad, path, 0);
	zip_stat_init(&stat);
	cb_assert(fd && !zip_stat(wad, path, 0, &stat), "WAD does not contain %s", path);

	char *data = cb_malloc((stat.size + 1) * sizeof *data);
	int data_len = zip_fread(fd, data, stat.size);
	data[data_len] = '\0';
	zip_fclose(fd);
	cb_assert(data_len);
	if (len) *len = data_len;

	cb_prof(CB_PROF_END, "WAD Load %s", path);
	return data;
}

void cb_wad_close(struct zip *wad) {
	cb_log(CB_LOG_DBUG, "Closing WAD...");
	zip_close(wad);
}
