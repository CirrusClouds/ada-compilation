section .text
   extern prints
   global main
dump:
   call prints
   ret
main:
   push 5
   push 12
   pop rax
   pop rbx
   add rax, rbx
   push rax
   call dump
   mov rax, 60
   mov rdi, 0
   syscall
