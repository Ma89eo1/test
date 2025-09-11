; Snake Game in 16-bit x86 Assembly for emu8086
; Features: VGA graphics, keyboard control, random food, score tracking
; Controls: Arrow keys (Up/Down/Left/Right), Esc to quit

.model small
.stack 100h
.data
    ; Game constants
    GRID_WIDTH  equ 20        ; Grid is 20x12 (320x200, 16x16 pixels/cell)
    GRID_HEIGHT equ 12
    SNAKE_MAX   equ 100       ; Max snake length
    VIDEO_SEG   equ 0A000h    ; VGA video memory
    PIXEL_SIZE  equ 16        ; Each cell is 16x16 pixels

    ; Colors
    COLOR_SNAKE equ 10        ; Green for snake
    COLOR_FOOD  equ 12        ; Red for food
    COLOR_BG    equ 0         ; Black background

    ; Snake data
    snake_x     dw SNAKE_MAX dup(0)  ; X coords (in cells)
    snake_y     dw SNAKE_MAX dup(0)  ; Y coords (in cells)
    snake_len   dw 3                 ; Initial length
    snake_dir   db 3                 ; 0=up, 1=down, 2=left, 3=right
    head_x      dw 10                ; Head start position
    head_y      dw 6
    food_x      dw 0                 ; Food position
    food_y      dw 0
    score       dw 0                 ; Score counter

    ; Messages
    msg_gameover db 'Game Over! Score: $'
    msg_score    db 5 dup(0), '$'    ; Buffer for score digits

.code
main proc
    mov ax, @data
    mov ds, ax

    ; Initialize VGA mode 13h (320x200, 256 colors)
    mov ax, 0013h
    int 10h

    ; Initialize snake at (10,6), moving right
    mov snake_x[0], 10
    mov snake_y[0], 6
    mov snake_x[2], 9
    mov snake_y[2], 6
    mov snake_x[4], 8
    mov snake_y[4], 6

    ; Place first food
    call spawn_food

game_loop:
    ; Clear keyboard buffer
    mov ah, 0Ch
    mov al, 0
    int 21h

    ; Check for keypress (non-blocking)
    mov ah, 01h
    int 16h
    jz no_key
    mov ah, 00h
    int 16h
    cmp ah, 01h         ; Esc key
    je game_over
    cmp ah, 48h         ; Up arrow
    je set_up
    cmp ah, 50h         ; Down arrow
    je set_down
    cmp ah, 4Bh         ; Left arrow
    je set_left
    cmp ah, 4Dh         ; Right arrow
    je set_right
    jmp no_key

set_up:
    cmp snake_dir, 1    ; Prevent reversing into self
    je no_key
    mov snake_dir, 0
    jmp no_key
set_down:
    cmp snake_dir, 0
    je no_key
    mov snake_dir, 1
    jmp no_key
set_left:
    cmp snake_dir, 3
    je no_key
    mov snake_dir, 2
    jmp no_key
set_right:
    cmp snake_dir, 2
    je no_key
    mov snake_dir, 3

no_key:
    ; Update snake position
    mov bx, snake_len
    shl bx, 1           ; Index for word array
    dec bx
    dec bx              ; Start from tail
tail_loop:
    cmp bx, 0
    jle move_head
    mov ax, snake_x[bx-2]
    mov snake_x[bx], ax
    mov ax, snake_y[bx-2]
    mov snake_y[bx], ax
    sub bx, 2
    jmp tail_loop

move_head:
    mov ax, head_x
    mov bx, head_y
    cmp snake_dir, 0    ; Up
    je move_up
    cmp snake_dir, 1    ; Down
    je move_down
    cmp snake_dir, 2    ; Left
    je move_left
    ; Right
    inc ax
    jmp update_pos
move_up:
    dec bx
    jmp update_pos
move_down:
    inc bx
    jmp update_pos
move_left:
    dec ax

update_pos:
    mov head_x, ax
    mov head_y, bx
    mov snake_x[0], ax
    mov snake_y[0], bx

    ; Check wall collision
    cmp ax, GRID_WIDTH
    jae game_over
    cmp ax, 0
    jl game_over
    cmp bx, GRID_HEIGHT
    jae game_over
    cmp bx, 0
    jl game_over

    ; Check self collision
    mov cx, snake_len
    mov si, 2           ; Start from second segment
self_coll_loop:
    cmp si, cx
    jge no_coll
    mov dx, snake_x[si]
    cmp ax, dx
    jne next_seg
    mov dx, snake_y[si]
    cmp bx, dx
    je game_over
next_seg:
    add si, 2
    jmp self_coll_loop

no_coll:
    ; Check food collision
    cmp ax, food_x
    jne no_food
    cmp bx, food_y
    jne no_food
    inc snake_len
    inc score
    call spawn_food

no_food:
    ; Clear screen (black)
    mov ax, VIDEO_SEG
    mov es, ax
    xor di, di
    mov cx, 320*200
    mov al, COLOR_BG
    rep stosb

    ; Draw snake
    mov cx, snake_len
    xor si, si
draw_snake:
    mov ax, snake_x[si]
    mov bx, snake_y[si]
    call draw_cell
    add si, 2
    loop draw_snake

    ; Draw food
    mov ax, food_x
    mov bx, food_y
    mov dl, COLOR_FOOD
    call draw_cell

    ; Delay (adjust for speed)
    mov cx, 0FFFFh
delay_loop:
    loop delay_loop

    jmp game_loop

game_over:
    ; Switch to text mode
    mov ax, 0003h
    int 10h

    ; Display game over and score
    mov dx, offset msg_gameover
    mov ah, 09h
    int 21h

    ; Convert score to string
    mov ax, score
    mov di, offset msg_score
    call num_to_str
    mov dx, offset msg_score
    mov ah, 09h
    int 21h

    ; Exit
    mov ax, 4C00h
    int 21h

main endp

; Draw 16x16 cell at (AX,BX) with color DL
draw_cell proc
    push ax
    push bx
    push cx
    push di
    shl ax, 4        ; AX *= 16 (pixel x)
    shl bx, 4        ; BX *= 16 (pixel y)
    mov di, bx
    imul di, 320     ; DI = y*320
    add di, ax       ; DI += x
    mov ax, VIDEO_SEG
    mov es, ax
    mov cx, PIXEL_SIZE
draw_row:
    push cx
    mov cx, PIXEL_SIZE
    mov al, dl
    rep stosb        ; Draw row of pixels
    add di, 320-PIXEL_SIZE
    pop cx
    loop draw_row
    pop di
    pop cx
    pop bx
    pop ax
    ret
draw_cell endp

; Spawn food at random position
spawn_food proc
    ; Simple PRNG using timer tick
    mov ah, 00h
    int 1Ah          ; Get timer ticks in DX
    mov ax, dx
    xor dx, dx
    mov cx, GRID_WIDTH
    div cx           ; AX % GRID_WIDTH
    mov food_x, dx
    mov ax, dx
    xor dx, dx
    mov cx, GRID_HEIGHT
    div cx
    mov food_y, dx
    ret
spawn_food endp

; Convert AX to decimal string at DI
num_to_str proc
    push ax
    push bx
    push cx
    push dx
    mov bx, 10
    mov cx, 0
num_loop:
    xor dx, dx
    div bx
    add dl, '0'
    push dx
    inc cx
    cmp ax, 0
    jne num_loop
store_loop:
    pop dx
    mov [di], dl
    inc di
    loop store_loop
    mov byte ptr [di], '$'
    pop dx
    pop cx
    pop bx
    pop ax
    ret
num_to_str endp

end main
