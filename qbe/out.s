.text
print:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, %esi
	leaq fmt(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
.type print, @function
.size print, .-print
/* end function print */

.data
.balign 8
fmt:
	.ascii "%d\n"
	.byte 0
/* end data */

.text
.globl foo1_2
foo1_2:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, %eax
	addl %esi, %eax
	leave
	ret
.type foo1_2, @function
.size foo1_2, .-foo1_2
/* end function foo1_2 */

.text
.globl t
t:
	pushq %rbp
	movq %rsp, %rbp
	movl $0, %eax
	leave
	ret
.type t, @function
.size t, .-t
/* end function t */

.data
.balign 8
z:
	.int 0
/* end data */

