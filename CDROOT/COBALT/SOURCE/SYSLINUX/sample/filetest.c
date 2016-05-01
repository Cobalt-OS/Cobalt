#include <com32.h>
#include <stdarg.h>

#define NULL ((void *)0)
int printf(const char *, ...);

static inline void memset(void *buf, int ch, unsigned int len)
{
  asm volatile("cld; rep; stosb"
               : "+D" (buf), "+c" (len) : "a" (ch) : "memory");
}

static void strcpy(char *dst, const char *src)
{
  while ( *src )
    *dst++ = *src++;

  *dst = '\0';
}

static void printregs(const com32sys_t *r)
{
  printf("eflags = %08x  ds = %04x  es = %04x  fs = %04x  gs = %04x\n"
	 "eax = %08x  ebx = %08x  ecx = %08x  edx = %08x\n"
	 "ebp = %08x  esi = %08x  edi = %08x  esp = %08x\n",
	 r->eflags.l, r->ds, r->es, r->fs, r->gs,
	 r->eax.l, r->ebx.l, r->ecx.l, r->edx.l,
	 r->ebp.l, r->esi.l, r->edi.l, r->_unused.l);
}

int __start(void)
{
  unsigned int ax,cx,dx,es,si,di,t;
  com32sys_t  inreg,outreg;
  char *p;
  
  /* Test null system call */
  inreg.eflags.l = 0xffffffff;
  inreg.eax.l = 0x11110000;
  inreg.ecx.l = 0x22222222;
  inreg.edx.l = 0x33333333;
  inreg.ebx.l = 0x44444444;
  inreg.ebp.l = 0x55555555;
  inreg.esi.l = 0x66666666;
  inreg.edi.l = 0x77777777;
  inreg.ds = 0xaaaa;
  inreg.es = 0xbbbb;
  inreg.fs = 0xcccc;
  inreg.gs = 0xdddd;
  printregs(&inreg);
  __com32.cs_intcall(0x22, &inreg, &outreg);
  printregs(&outreg);
  printf("----\n");

  memset(&inreg, 0, sizeof inreg);
  memset(&outreg, 0, sizeof outreg);
  strcpy(__com32.cs_bounce, "test.txt");
  inreg.eax.w[0] = 0x0006;  // Open file
  inreg.esi.w[0] = OFFS(__com32.cs_bounce);
  inreg.es = SEG(__com32.cs_bounce);
  printregs(&inreg);
  __com32.cs_intcall(0x22, &inreg, &outreg);
  printregs(&outreg);
  printf("----\n");
  
  si = outreg.esi.w[0];		/* File handle */
  cx = outreg.ecx.w[0];		/* Block size */
  ax = outreg.eax.l;		/* File length */

  while ( si ) {
    /* We can only read 64K per call */
    t = ( ax > 65536 ) ? 65536/cx : (ax+cx-1)/cx;
    
    memset(&inreg, 0, sizeof inreg);
    inreg.esi.w[0] = si;
    inreg.ecx.w[0] = t;		/* Block count */
    inreg.eax.w[0] = 0x0007;  // Read file
    inreg.ebx.w[0] = OFFS(__com32.cs_bounce);
    inreg.es = SEG(__com32.cs_bounce);
    printregs(&inreg);
    __com32.cs_intcall(0x22, &inreg, &outreg);
    printregs(&outreg);
    printf("----\n");
    si = outreg.esi.w[0];

    /* Print the buffer */
    t = (ax < 65536) ? ax : 65536;
    p = __com32.cs_bounce;
    while ( t ) {
      putchar(*p++);
      t--;
      ax--;
    }
  }

  return 0;
}
