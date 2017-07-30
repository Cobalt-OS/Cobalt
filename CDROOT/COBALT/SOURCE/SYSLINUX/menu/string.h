
#ifndef __STRING_H__
#define __STRING_H__

/* String routines */

void *memset(void *buf, int chr, unsigned int len);

char *strcpy(char *dst, const char *src);

char *strcat(char *dst, const char * src);

int strcmp(const char *a,const char*b);

int strlen(const char *a);

#endif
