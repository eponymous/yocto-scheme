#include <stdint.h>
#include <stdbool.h>

/* LC_ALL = "C.UTF-8" */

bool utf8_isalnum(const uint32_t c);
bool utf8_isalpha(const uint32_t c);
bool utf8_isblank(const uint32_t c);
bool utf8_iscntrl(const uint32_t c);
bool utf8_isdigit(const uint32_t c);
bool utf8_isgraph(const uint32_t c);
bool utf8_islower(const uint32_t c);
bool utf8_isprint(const uint32_t c);
bool utf8_ispunct(const uint32_t c);
bool utf8_isspace(const uint32_t c);
bool utf8_isupper(const uint32_t c);
bool utf8_isxdigit(const uint32_t c);

uint32_t utf8_toupper(const uint32_t c);
uint32_t utf8_tolower(const uint32_t c);
