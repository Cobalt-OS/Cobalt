# $Id: memcpy.S,v 1.1 2003/04/15 19:31:04 hpa Exp $
#
# memcpy.S
#
# Simple memcpy() implementation
#

	.text
	.globl memcpy
	.type memcpy, @function
memcpy:
	cld
	pushl %edi
	pushl %esi
	movl 12(%esp),%edi
	movl 16(%esp),%esi
	movl 20(%esp),%eax
	movl %eax,%ecx
	shrl $2,%ecx
	rep ; movsl
	movl %eax,%ecx
	andl $3,%ecx
	rep ; movsb
	movl 12(%esp),%eax
	popl %esi
	popl %edi
	ret

	.size memcpy,.-memcpy
