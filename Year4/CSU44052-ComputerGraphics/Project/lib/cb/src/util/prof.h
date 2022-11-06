#ifndef CBI_UTIL_PROF_H
#define CBI_UTIL_PROF_H

enum cbi_prof_event {
	CBI_PROF_EVENT,
	CBI_PROF_START,
	CBI_PROF_END,
	cbi_prof_event_len
};

#ifndef CB_NO_PROF
void cbi_prof_start(void);
void cbi_prof(int line, const char *func, const char *file, enum cbi_prof_event event, ...);
void cbi_prof_finish(void);
void cbi_prof_toggle(void);
#define cbi_prof(...) cbi_prof(__LINE__, __func__, __FILE__, __VA_ARGS__, NULL)
#else
#define cbi_prof_start()
#define cbi_prof(...) (__VA_ARGS__)
#define cbi_prof_finish()
#define cbi_prof_toggle()
#endif

#endif
