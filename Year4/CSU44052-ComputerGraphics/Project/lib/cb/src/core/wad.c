#include "wad.h"
#include "../cfg.h"
#include "../util/alloc.h"
#include "../util/assert.h"
#include "../util/prof.h"
#include "../util/log.h"
#include <string.h>
#include <libgen.h>
#include <zip.h>

struct cbi_wad {
	struct zip *zip;
	char *path;
};

struct cbi_wad *cbi_wad_open(char *exe_path) {
	cbi_log(CB_LOG_DBUG, "Opening WAD...");
	struct cbi_wad *wad = cbi_malloc(sizeof *wad);

	int err;
	wad->path = dirname(strdup(exe_path));
	wad->path = strcat(cbi_realloc(wad->path, strlen(wad->path) + strlen(CB_WAD) + 1), CB_WAD);
	wad->zip = zip_open(wad->path, ZIP_RDONLY, &err);
	cbi_assert(wad->zip);

	cbi_log(CB_LOG_INFO, "Using WAD %s", wad->path);
	return wad;
}

char *cbi_wad_load(struct cbi_wad *wad, char *path, int *len) {
	cbi_prof(CBI_PROF_START, "WAD Load %s", path);
	cbi_log(CB_LOG_DBUG, "Loading %s from WAD...", path);

	struct zip_stat stat;
	struct zip_file *fd = zip_fopen(wad->zip, path, 0);
	zip_stat_init(&stat);
	cbi_assert(fd && !zip_stat(wad->zip, path, 0, &stat), "WAD does not contain %s", path);

	char *data = cbi_malloc((stat.size + 1) * sizeof *data);
	int data_len = zip_fread(fd, data, stat.size);
	data[data_len] = '\0';
	zip_fclose(fd);
	cbi_assert(data_len);

	if (len)
		*len = data_len;

	cbi_prof(CBI_PROF_END, "WAD Load %s", path);
	return data;
}

void cbi_wad_close(struct cbi_wad *wad) {
	cbi_log(CB_LOG_DBUG, "Closing WAD...");
	// TODO(wad): unload all loaded resources
	zip_close(wad->zip);
	cbi_free(wad->path);
	cbi_free(wad);
}
