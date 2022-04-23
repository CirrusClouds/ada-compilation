section .text
    default rel
    extern printf
    global prints: function
prints:        
    push rbp
    ;; sub rsp, 32
        
    mov	rdi, fmt
    mov	rsi, msg
    mov	rax, 0

    ; Call printf
    call printf wrt ..plt
    
    pop	rbp		; Pop stack

    ;; mov	rax,0	; Exit code 0
    ret			; Return

section .data
    msg:  db        "todo: dump", 10, 0
    fmt:    db "%s", 10, 0
