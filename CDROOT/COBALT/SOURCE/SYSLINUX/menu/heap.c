/*
 * Very simple heap manager.
 * Allocation Strategy: The lower end of the heap, is the end of the BSS.
 *
 * During an alloc, if there is enough space below the high end of heap, 
 *    we return a pointer to the allocated space and move curr.
 * Space once allocated is never deallocated!
 * We run out of space if we get within BUFSIZE bytes of the stack pointer.
 */

#include "heap.h"
#include "string.h"
#include "biosio.h"

#ifndef NULL
#define NULL ((void *)0)
#endif

extern char _end[];
static unsigned int heap_curr  = (unsigned int)_end;

static inline unsigned int currsp(void)
{
    unsigned int esp;
    
    asm("movl %%esp,%0 " : "=rm" (esp));
    return esp;
}

static inline void _checkheap(void)
{
    if (currsp() < heap_curr) // Heap corrupted
    {
	csprint("\r\nHeap overflow, aborting!\r\n",0x07);
	asm volatile("int $0x21" : : "a" (0x4C7f)); /* Exit with error */
	return;
    }
}

void * malloc(unsigned int num) // Allocate so much space
{
    unsigned int ans, heap_max;

    _checkheap();
    heap_max = currsp() - STACKSIZE;

    ans = (heap_curr+3) & ~3;	// Align to 4-byte boundary

    if ( ans+num > heap_max )
	return NULL;

    heap_curr = ans+num;
    return (void *) ans;
}

/* We don't actually ever use these; if enabled,
   probably _checkheap() shouldn't be inline.*/
#if 0

int checkalloc(unsigned int num)
{
    _checkheap();
    return (heap_curr + num < heap_max);
}


void free(void * ptr)
{
    _checkheap();
    return;
}

#endif
