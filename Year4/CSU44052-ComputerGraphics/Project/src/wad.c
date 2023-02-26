#include "wad.h"
#include "assert.h"
#include "alloc.h"
#include "config.h"
#include "log.h"
#include <string.h>
#include <libgen.h>
#include <zip.h>

extern struct zip *wad;

void ww_wad_open(char *exe) {
	ww_log(WW_LOG_DBUG, "Opening WAD...");
	char *path = dirname(strdup(exe));
	path = strcat(ww_realloc(path, strlen(path) + strlen(WW_WAD_FILE) + 1), WW_WAD_FILE);
	wad = zip_open(path, ZIP_RDONLY, NULL);
	ww_assert(wad, "Unable to open WAD %s", path);
	ww_log(WW_LOG_INFO, "Using WAD %s", path);
	ww_free(path);
}

char *ww_wad(char *path, int *len) {
	ww_log(WW_LOG_DBUG, "Loading %s...", path);
	struct zip_file *fd = zip_fopen(wad, path, 0);

	struct zip_stat stat;
	zip_stat_init(&stat);
	ww_assert(fd && !zip_stat(wad, path, 0, &stat), "WAD does not contain %s", path);

	char *data = ww_malloc((stat.size + 1) * sizeof *data);
	int data_len = zip_fread(fd, data, stat.size);
	data[data_len] = '\0';

	zip_fclose(fd);
	ww_assert(data_len, "Unable to load any data from %s", path);
	if (len) *len = data_len;
	return data;
}

void ww_wad_close(void) {
	zip_close(wad);
}
