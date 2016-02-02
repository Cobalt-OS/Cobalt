
#ifndef _HEAP_H_
#define _HEAP_H_

// How much space to reserve for the stack
#define STACKSIZE (8*1024)

// Will an allocation of num bytes be successful?
// We need this because we dont do any deallocation
extern int checkalloc(unsigned int num); 

// Allocate so much space
extern void * malloc(unsigned int num); 

// This is a nop for now may be future implementations will actually do something
extern void free(void *); // Dealloc space. 

#endif

