org 100h
jmp start

; === MESSAGES ===
prompt db 'Calc8086> $'
unknown db 13,10,'Unknown command.',13,10,'$'
helpmsg db 13,10,'Commands: ADD, SUB, MUL, DIV, HISTORY, MEM, CLEAR, HELP, EXIT',13,10,'$'
inputmsg db 13,10,'Enter number: $'
newline db 13,10,'$'

buffer db 32 dup('$')
history db 10 dup(16 dup('$')) ; store last 10 operations
hist_count db 0
memory dw 0

start:
    mov ax,@data
    mov ds,ax

main_loop:
    mov ah,09h
    mov dx,offset prompt
    int 21h

    lea si,buffer
    call read_line
    lea si,buffer
    call to_upper

    lea si,buffer
    call compare_add    je do_add
    lea si,buffer
    call compare_sub    je do_sub
    lea si,buffer
    call compare_mul    je do_mul
    lea si,buffer
    call compare_div    je do_div
    lea si,buffer
    call compare_history je do_history
    lea si,buffer
    call compare_mem    je do_mem
    lea si,buffer
    call compare_clear  je do_clear
    lea si,buffer
    call compare_help   je do_help
    lea si,buffer
    call compare_exit   je do_exit

    mov dx,offset unknown
    mov ah,09h
    int 21h
    jmp main_loop

; === COMMANDS ===

do_add:
    call get_number
    mov bx,ax
    call get_number
    add ax,bx
    mov memory,ax
    call print_number
    call store_history
    jmp main_loop

do_sub:
    call get_number
    mov bx,ax
    call get_number
    sub bx,ax
    mov ax,bx
    mov memory,ax
    call print_number
    call store_history
    jmp main_loop

do_mul:
    call get_number
    mov bx,ax
    call get_number
    mul bx
    mov memory,ax
    call print_number
    call store_history
    jmp main_loop

do_div:
    call get_number
    mov bx,ax
    call get_number
    xor dx,dx
    div bx
    mov memory,ax
    call print_number
    call store_history
    jmp main_loop

do_history:
    mov cl,hist_count
    cmp cl,0
    je .empty
    mov si,offset history
    mov cx,cl
.print_hist:
    mov dx,si
    mov ah,09h
    int 21h
    add si,16
    loop .print_hist
    jmp main_loop
.empty:
    mov dx,offset newline
    mov ah,09h
    int 21h
    jmp main_loop

do_mem:
    mov ax,memory
    call print_number
    jmp main_loop

do_clear:
    mov hist_count,0
    mov memory,0
    jmp main_loop

do_help:
    mov dx,offset helpmsg
    mov ah,09h
    int 21h
    jmp main_loop

do_exit:
    ret

; === UTILITIES ===

read_line:
    mov cx,0
.rl_loop:
    mov ah,01h
    int 21h
    cmp al,13
    je .rl_done
    mov [si],al
    inc si
    inc cx
    cmp cx,31
    je .rl_done
    jmp .rl_loop
.rl_done:
    mov [si],'$'
    ret

to_upper:
    mov cx,32
.upper_loop:
    mov al,[si]
    cmp al,'a'
    jb .next_upper
    cmp al,'z'
    ja .next_upper
    sub al,32
    mov [si],al
.next_upper:
    inc si
    loop .upper_loop
    ret

; === NUMBER INPUT ===
get_number:
    mov dx,offset inputmsg
    mov ah,09h
    int 21h
    xor ax,ax
    xor bx,bx
.read_digit:
    mov ah,01h
    int 21h
    cmp al,13
    je .done
    sub al,'0'
    mov bl,10
    mul bl
    add ax,al
    jmp .read_digit
.done:
    ret

; === PRINT NUMBER ===
print_number:
    push ax bx cx dx
    mov bx,10
    xor cx,cx
.p1:
    xor dx,dx
    div bx
    push dx
    inc cx
    test ax,ax
    jnz .p1
.print_loop:
    pop dx
    add dl,'0'
    mov ah,02h
    int 21h
    loop .print_loop
    mov dl,13
    mov ah,02h
    int 21h
    mov dl,10
    mov ah,02h
    int 21h
    pop dx cx bx ax
    ret

store_history:
    mov cl,hist_count
    cmp cl,10
    jae .shift
    mov si,offset history
    add si,cl*16
    mov cx,16
.copy:
    mov al,0
    mov [si],al
    inc si
    loop .copy
    inc hist_count
    ret
.shift:
    ; shift old history up
    ret

; === COMPARE ===
compare_add:    mov di,offset cmd_add  call compare  ret
compare_sub:    mov di,offset cmd_sub  call compare  ret
compare_mul:    mov di,offset cmd_mul  call compare  ret
compare_div:    mov di,offset cmd_div  call compare  ret
compare_history:mov di,offset cmd_history call compare ret
compare_mem:    mov di,offset cmd_mem  call compare  ret
compare_clear:  mov di,offset cmd_clear call compare  ret
compare_help:   mov di,offset cmd_help call compare  ret
compare_exit:   mov di,offset cmd_exit call compare  ret

compare:
.loop:
    mov al,[si]
    cmp al,0
    je .match
    cmp al,[di]
    jne .fail
    inc si
    inc di
    jmp .loop
.match: mov ax,1 ret
.fail:  mov ax,0 ret

cmd_add db 'ADD',0
cmd_sub db 'SUB',0
cmd_mul db 'MUL',0
cmd_div db 'DIV',0
cmd_history db 'HISTORY',0
cmd_mem db 'MEM',0
cmd_clear db 'CLEAR',0
cmd_help db 'HELP',0
cmd_exit db 'EXIT',0


end main
