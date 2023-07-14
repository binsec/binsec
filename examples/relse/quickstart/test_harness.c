#include <stdlib.h>

int memcmp(const void *s1, const void *s2, size_t n);

#define SIZE (1 << 4) /* 16B */
char s1[SIZE], s2[SIZE];
size_t n = SIZE;

int main(int argc, char *argv[])
{
  int res = memcmp(s1, s2, n);

  /* ensure the result is in [-1..1] */
  res |= res >> 1;
  res |= res >> 2;
  res |= res >> 4;
  res |= res >> 8;
  res |= res >> 16;
  res = (res & 1) | (res >> 31);

  exit(res);
}
