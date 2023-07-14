#include <stddef.h>

int memcmp(const void *s1, const void *s2, size_t n)
{
    const char *p1 = (const char *)s1, *p2 =(const char *)s2;
    for (size_t i = 0; i < n; i += 1) {
        if (p1[i] < p2[i]) return -1;
        else if (p1[i] > p2[i]) return 1;
    }
    return 0;
}
