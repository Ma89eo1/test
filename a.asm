org 100h
jmp start

; === MESSAGES ===
prompt db 'MiniFS> $'
unknown db 13,10,'Unknown command.',13,10,'$'
helpmsg db 13,10,'Commands: NEW, LIST, VIEW, DEL, HELP, EXIT',13,10,'$'
entername db 13,10,'Enter filename: $'
entercontent db 13,10,'Enter content: $'
filenotfound db 13,10,'File not found.',13,10,'$'
newline db 13,10,'$'

buffer db 32 dup('$')
file_names db 10 dup(8 dup('$'))  ; 10 filenames, 8 chars each
file_contents db 10 dup(32 dup('$')) ; corresponding content slots

start:
    mov ax, @data
    mov ds, ax

main_loop:
    mov ah, 09h
    mov dx, offset prompt
    int 21h

    lea si, buffer
    call read_line
    lea si, buffer
    call to_upper

    lea si, buffer
    call compare_new    je do_new
    lea si, buffer
    call compare_list   je do_list
    lea si, buffer
    call compare_view   je do_view
    lea si, buffer
    call compare_del    je do_del
    lea si, buffer
    call compare_help   je do_help
    lea si, buffer
    call compare_exit   je do_exit

    mov dx, offset unknown
    mov ah, 09h
    int 21h
    jmp main_loop

; === COMMANDS ===

do_new:
    mov dx, offset entername
    mov ah, 09h
    int 21h
    lea si, buffer
    call read_line
    ; save filename
    mov di, offset file_names
    mov cx, 10
.findslot:
    mov bx, 8
    mov dx, di
    mov si, offset buffer
    call copy_string
    jmp main_loop

do_list:
    mov cx, 10
    mov di, offset file_names
.list_loop:
    mov dx, di
    mov ah, 09h
    int 21h
    add di, 8
    loop .list_loop
    jmp main_loop

do_view:
    mov dx, offset entername
    mov ah, 09h
    int 21h
    lea si, buffer
    call read_line
    ; search filename
    mov di, offset file_names
    mov cx, 10
    mov bx, -1
.search_loop:
    mov si, offset buffer
    call compare_string
    jnz .found
    add di, 8
    loop .search_loop
    mov dx, offset filenotfound
    mov ah, 09h
    int 21h
    jmp main_loop
.found:
    ; display content
    mov dx, offset file_contents
    int 21h
    jmp main_loop

do_del:
    mov dx, offset entername
    mov ah, 09h
    int 21h
    lea si, buffer
    call read_line
    ; delete logic here (optional)
    jmp main_loop

do_help:
    mov dx, offset helpmsg
    mov ah, 09h
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
    je .done_read
    mov [si], al
    inc si
    inc cx
    cmp cx,31
    je .done_read
    jmp .rl_loop
.done_read:
    mov [si],'$'
    ret

to_upper:
    mov cx,32
.upper_loop:
    mov al,[si]
    cmp al,'a'
    jb .next
    cmp al,'z'
    ja .next
    sub al,32
    mov [si],al
.next:
    inc si
    loop .upper_loop
    ret

compare_new:  mov di,offset cmd_new  call compare  ret
compare_list: mov di,offset cmd_list call compare  ret
compare_view: mov di,offset cmd_view call compare  ret
compare_del:  mov di,offset cmd_del  call compare  ret
compare_help: mov di,offset cmd_help call compare  ret
compare_exit: mov di,offset cmd_exit call compare  ret

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

copy_string:
    push cx dx
.copy_loop:
    mov al,[si]
    mov [dx],al
    inc si
    inc dx
    dec bx
    jnz .copy_loop
    pop dx cx
    ret

cmd_new db 'NEW',0
cmd_list db 'LIST',0
cmd_view db 'VIEW',0
cmd_del db 'DEL',0
cmd_help db 'HELP',0
cmd_exit db 'EXIT',0
