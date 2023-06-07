#ifndef CB_WAD_H
#define CB_WAD_H

struct zip *cb_wad_open(char *exe_path);
char *cb_wad_load(struct zip *wad, char *path, int *len);
void cb_wad_close(struct zip *wad);

#endif
