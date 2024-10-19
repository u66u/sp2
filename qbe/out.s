.data
.balign 8
format_str:
	.ascii "%d "
	.byte 0
/* end data */

.text
.globl rand
rand:
	pushq %rbp
	movq %rsp, %rbp
	imull $1103515245, %edi, %eax
	addl $12345, %eax
	movl $2147483648, %ecx
	movl $0, %edx
	divl %ecx
	movl %edx, %eax
	leave
	ret
.type rand, @function
.size rand, .-rand
/* end function rand */

