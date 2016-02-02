# $Id#
#
# Implementation of exit() for com32 based on c32entry.S
#
	.text
	.globl exit
exit:
	movl 4(%esp),%eax		# Exit code in %eax = return value
	movl (__entry_esp),%esp		# Return stack pointer to entry value
	ret				# Return to termination address
