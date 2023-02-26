#ifndef WW_WAD_H
#define WW_WAD_H

void ww_wad_open(char *exe);
char *ww_wad(char *path, int *len);
void ww_wad_close(void);

#endif
