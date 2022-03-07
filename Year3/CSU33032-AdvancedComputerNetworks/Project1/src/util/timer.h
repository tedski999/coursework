#ifndef WP_UTIL_TIMER_H
#define WP_UTIL_TIMER_H

struct wp_timer;

struct wp_timer *wp_timer_create(void);
double wp_timer_measure(const struct wp_timer *timer);
void wp_timer_set(struct wp_timer *timer, double new_time);
void wp_timer_destroy(struct wp_timer *timer);

#endif
