# $Id: memset.S,v 1.1 2003/04/14 22:28:30 hpa Exp $
#
# memset.S
#
# Simple memset() implementation
#

	.text
	.globl memset
	.type memset, @function
memset:
	cld
	pushl %edi
	pushl %esi
	movl 12(%esp),%edi
	movzbl 16(%esp),%eax
	movl 20(%esp),%esi
	imull $0x01010101,%eax
	movl %esi,%ecx
	shrl $2,%ecx
	rep ; stosl
	movl %esi,%ecx
	andl $3,%ecx
	rep ; stosb
	movl 12(%esp),%eax
	popl %esi
	popl %edi
	ret

	.size memcpy,.-memcpy
