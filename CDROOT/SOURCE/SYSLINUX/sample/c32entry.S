#ident "$Id: c32entry.S,v 1.4 2003/12/13 02:22:56 hpa Exp $"
# -----------------------------------------------------------------------
#   
#   Copyright 2003 H. Peter Anvin - All Rights Reserved
#
#   Permission is hereby granted, free of charge, to any person
#   obtaining a copy of this software and associated documentation
#   files (the "Software"), to deal in the Software without
#   restriction, including without limitation the rights to use,
#   copy, modify, merge, publish, distribute, sublicense, and/or
#   sell copies of the Software, and to permit persons to whom
#   the Software is furnished to do so, subject to the following
#   conditions:
#   
#   The above copyright notice and this permission notice shall
#   be included in all copies or substantial portions of the Software.
#   
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#   OTHER DEALINGS IN THE SOFTWARE.
#
# -----------------------------------------------------------------------

# COM32 start up code - must be linked first in the binary

	
		.section ".text","ax"
		.globl _start
_start:
		# This first instruction acts as COM32 magic number
		movl $0x21cd4cff,%eax

		# Upwards string operations
		cld

		# Zero the .bss segment
		xorl %eax,%eax
		movl $__bss_start,%edi		# Symbol provided by linker
		movl $_end+3,%ecx		# Symbol provided by linker
		subl %edi,%ecx
		shrl $2,%ecx
		rep ; stosl

		# Copy COM32 invocation parameters
		leal 4(%esp),%esi		# Argument list
		movl $__com32,%edi
		movl $6,%ecx
		movl %esp,-4(%edi)		# Save the initial stack ptr
		cmpl (%esi),%ecx
		jbe 1f
		movl (%esi),%ecx
1:		rep ; movsl

		# Run program; we call this __start rather than main since we
		# did not parse the command line or anything like that.
		jmp __start

		.section ".bss","a"
		.globl __entry_esp
__entry_esp:	.space 4
		.globl __com32
__com32:	.space 4*6
