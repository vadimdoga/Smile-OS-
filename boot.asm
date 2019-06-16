org 0x7C00
bits 16

mov ah,00h
mov al,3
int 10h

mov [BOOT_DRIVE], dl

call load_boot
call execute_boot

load_boot:
	pusha
	mov bx, LOADING_BOOT_MSG
	call print_string
	mov bx, 0x1000
	mov dh, 15
	mov dl, [BOOT_DRIVE]
	call disk_read
	popa
	ret

execute_boot:
	mov bx, ENTER_BOOT_MSG
	call print_string
    call waitKeyboard
    
waitKeyboard:
    mov ah,00h
    int 16h
    cmp ah,1ch
    je 0x1000
    jmp waitKeyboard

disk_read:
	pusha
	push dx

	mov ah, 0x02
	mov al, dh
	mov ch, 11
	mov dh, 0
	mov cl, 8
	int 0x13

	jc disk_read_error

	pop dx
	cmp dh, al
	jne disk_read_error

	popa
	ret

disk_read_error:
	mov bx, DISK_READ_ERROR_MSG
	call print_string
	hlt

print_string:
	pusha

	.loop:
	mov al, [bx]
	cmp al, 0
	je .ret
	mov ah, 0x0E
	int 0x10
	inc bx
	jmp .loop

	.ret:
	mov ah, 0x0E
	mov al, 0x0A
	int 0x10
	mov al, 0x0D
	int 0x10

	popa
	ret

BOOT_DRIVE: db 0
LOADING_BOOT_MSG: db "Loading Boot", 0
ENTER_BOOT_MSG:  db "Press Enter to continue....", 0
DISK_READ_ERROR_MSG: db "Disk Error", 0

times 510 - ($ - $$) db 0
dw 0xAA55
; times 510 - ($ - $$) db 0
; dw 0xAA55
; db 0x55 ;byte 511 = 0x55
; db 0xAA ;byte 512 = 0xAA
; times 205,312 - ($ - $$) db 0