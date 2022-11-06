#ifndef CBI_CORE_WAD_H
#define CBI_CORE_WAD_H

struct cbi_wad *cbi_wad_open(char *exe_path);
char *cbi_wad_load(struct cbi_wad *wad, char *path, int *len);
void cbi_wad_close(struct cbi_wad *wad);

#endif
