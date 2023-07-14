#include <stddef.h>

int memcmp(const void *s1, const void *s2, size_t n)
{
  const char *p1 = (const char *)s1, *p2 = (const char *)s2;
  int res = 0;
  for (size_t i = 0; i < n; i += 1) {
    res = res | (~(res >> 31) & ((res - 1) >> 31) & (p1[i] - p2[i]));
  }
  return res;
}
