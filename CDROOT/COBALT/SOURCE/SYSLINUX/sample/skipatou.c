static inline int
isdigit(int ch)
{
  return (ch >= '0') && (ch <= '9');
}

unsigned int skip_atou(const char **s)
{
  int i=0;
  
  while (isdigit(**s))
    i = i*10 + *((*s)++) - '0';
  return i;
}
