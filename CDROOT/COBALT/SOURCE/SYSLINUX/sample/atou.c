static inline int
isdigit(int ch)
{
  return (ch >= '0') && (ch <= '9');
}

unsigned int atou(const char *s)
{
  unsigned int i = 0;
  while (isdigit(*s))
    i = i*10 + (*s++ - '0');
  return i;
}

