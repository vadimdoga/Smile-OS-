bits 16
org 0x1000

jmp start
;start
start:
	cli

	mov ah,00h
	mov al,03h
	int 10h

	mov sp,0x7c00

	mov si,greetings
	call printString

    mov ah,04h
    int  0x1A     ; call service 00 of RTC BIOS interrupt
    mov [storeTime1],dx
    mov ah,04h
    int  0x1A     ; call service 00 of RTC BIOS interrupt
    mov [storeTime3],cl
    mov ah,02h
    int 1ah
    mov [storeTime2],cx

    jmp mainLoop
;compare string with help command
compare_string_SI_BX_HELP:
	push si
	push bx
	push ax
compareHelp:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroHelp

	inc si
	inc bx

	jmp compareHelp
firstZeroHelp:
	cmp byte [bx],0
	jne notEqual

	mov cx,1

	pop si
	pop bx
	pop ax
	ret

;compare string with empty_space command
compare_string_SI_BX_EMPTY_SPACE:
	push si
	push bx
	push ax
compareEmptySpace:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroEmptySpace

	inc si
	inc bx

	jmp compareEmptySpace
firstZeroEmptySpace:
	cmp byte [bx],0
	jne notEqual

	mov cx,2

	pop si
	pop bx
	pop ax
	ret

;compare string with exit command
compare_string_SI_BX_SHUTDOWN:
	push si
	push bx
	push ax
compareShutdown:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroShutdown

	inc si
	inc bx

	jmp compareShutdown
firstZeroShutdown:
	cmp byte [bx],0
	jne notEqual

	mov cx,3

	pop si
	pop bx
	pop ax
	ret

;compare string with draw command
compare_string_SI_BX_DRAW:
	push si
	push bx
	push ax
compareDraw:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroDraw

	inc si
	inc bx

	jmp compareDraw
firstZeroDraw:
	cmp byte [bx],0
	jne notEqual

	mov cx,4

	pop si
	pop bx
	pop ax
	ret

;compare string with about command
compare_string_SI_BX_ABOUT:
	push si
	push bx
	push ax
compareAbout:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroAbout

	inc si
	inc bx

	jmp compareAbout
firstZeroAbout:
	cmp byte [bx],0
	jne notEqual

	mov cx,5

	pop si
	pop bx
	pop ax
	ret

;compare string with clear command
compare_string_SI_BX_CLEAR:
	push si
	push bx
	push ax
compareClear:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroClear

	inc si
	inc bx

	jmp compareClear
firstZeroClear:
	cmp byte [bx],0
	jne notEqual

	mov cx,6

	pop si
	pop bx
	pop ax
	ret

;compare string with ghost command
compare_string_SI_BX_GHOST:
	push si
	push bx
	push ax
compareGhost:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroGhost

	inc si
	inc bx

	jmp compareGhost
firstZeroGhost:
	cmp byte [bx],0
	jne notEqual

	mov cx,7

	pop si
	pop bx
	pop ax
	ret

;compare string with restart command
compare_string_SI_BX_RESTART:
	push si
	push bx
	push ax
compareRestart:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroRestart

	inc si
	inc bx

	jmp compareRestart
firstZeroRestart:
	cmp byte [bx],0
	jne notEqual

	mov cx,8

	pop si
	pop bx
	pop ax
	ret

;compare string with math command
compare_string_SI_BX_MATH:
	push si
	push bx
	push ax
compareMath:
	mov ah,[bx]
	cmp [si],ah
	jne notEqual

	cmp byte [si],0
	je firstZeroMath

	inc si
	inc bx

	jmp compareMath
firstZeroMath:
	cmp byte [bx],0
	jne notEqual

	mov cx,9

	pop si
	pop bx
	pop ax
	ret
;if si not equal to bx
notEqual:
    mov cx,0

	pop si
	pop bx
	pop ax
	ret
;print strings
printString:
    push ax                   ; save ax to stack

    mov ah, 0x0e              ; call tty function
    call printChar      

    pop ax                    ; take ax out of the stack
    ret                      
printChar:
    mov al, [si]              ; load symbol

    cmp al, 0                 ; if si empty then jump
    jz ifZero               

    int 0x10                  ; if not print al
    inc si                    ; increment si
    

    jmp printChar       ;again untill stack empty
ifZero:
    ret

mainLoop:
	mov si,prompt_message
	call printString

	call getInput

	jmp mainLoop
;check if enter or other button pressed
getInput:
	mov bx,0
inputFieldButtons:
	mov ah,0x0
	int 16h

	cmp al, 0x0d              ; if enter
    je enterPressed

	cmp al,0x8
	je backspacePressed

	mov ah,0eh
	int 10h

	mov [input + bx],al
	inc bx
 
    jmp inputFieldButtons   
;check the input for commands like help
enterPressed:
    mov byte [input + bx], 0    ; 0 at the end is the end of the line

    mov si, newLine          ; print new line
    call printString

    ;help command
	mov si,help_command
	mov bx,input
	call compare_string_SI_BX_HELP
	;compare if help was written
    cmp cx,1
	je equalHelp

    ;empty_space command
    mov si,empty_space
    mov bx,input
    call compare_string_SI_BX_EMPTY_SPACE
    ;compare if empty space pressed
    cmp cx,2
    je equalEmptySpace

    ;exit command
    mov si,shutdown_command
    mov bx,input
    call compare_string_SI_BX_SHUTDOWN
    ;compare if shutdown was written
    cmp cx,3
    je equalShutdown

    ;draw command
    mov si,draw_command
    mov bx,input
    call compare_string_SI_BX_DRAW
    ;compare if draw was written
    cmp cx,4
    je equalDraw

    ;about command
    mov si,about_command
    mov bx,input
    call compare_string_SI_BX_ABOUT
    ;compare if about was written
    cmp cx,5
    je equalAbout

    ;clear command
    mov si,clear_command
    mov bx,input
    call compare_string_SI_BX_CLEAR
    ;compare if clear was written
    cmp cx,6
    je equalClear

    ;ghost command
    mov si,ghost_command
    mov bx,input
    call compare_string_SI_BX_GHOST
    ;compare if ghost was written
    cmp cx,7
    je equalGhost

    ;restart command
    mov si,restart_command
    mov bx,input
    call compare_string_SI_BX_RESTART
    ;compare if restart was written
    cmp cx,8
    je equalRestart

    ;math command
    mov si,math_command
    mov bx,input
    call compare_string_SI_BX_MATH
    ;compare if math was written
    cmp cx,9
    je equalMath

	jmp equalNothing

;if math typed
returnEnter:
    ret
printStringDouble:
    push ax                   ; save ax to stack

    mov ah, 0x0e              ; call tty function
    call printCharDouble      

    pop ax                    ; take ax out of the stack
    ret                      
printCharDouble:
    mov al, [si + bp]              ; load symbol

    cmp al, 0                 ; if si empty then jump
    jz ifZero               

    int 0x10                  ; if not print al
    add bp,2                    ; increment si
    
    jmp printCharDouble       ;again untill stack empty
printStringMath:
    push ax                   ; save ax to stack

    mov ah, 0x0e              ; call tty function
    call printCharMath      

    pop ax                    ; take ax out of the stack
    ret                      
printCharMath:
    mov al, [si + bp]              ; load symbol
    cmp al, 0                 ; if si empty then jump
    jz ifZero               

    int 0x10                  ; if not print al
    sub bp,2                    ; increment si
    
    jmp printCharMath       ;again untill stack empty
ifEqualTwo:
    mov bp,0
    call printStringDouble
    jmp back0
ifEqualFour:
    mov bp,2
    call printStringMath
    jmp General
firstVariableKeyboard:
    mov ah,0x0
    int 16h

    cmp al,0x0d
    je returnEnter

    cmp al,0x8
	je backspacePressed

    mov ah,0eh
    int 10h

    mov [var1 + bp],al
    mov cx,[var1 + bp]

    mov word[var1 + bp],cx
    add bp,2
    add di,2
    mov cx,0

    jmp firstVariableKeyboard
secondVariableKeyboard:
    mov ah,0x0
    int 16h

    cmp al,0x0d
    je returnEnter

    cmp al,0x8
	je backspacePressed

    mov ah,0eh
    int 10h

    mov [var2 + bp],al
    mov cx,[var2 + bp]

    mov word[var2 + bp],cx
    add bl,2
    add bp,2
    mov cx,0

    jmp secondVariableKeyboard
chooseOperator:
    mov ah,00h
    int 16h
    cmp ah,2
    je Addition
    cmp ah,3
    je Subtraction
    cmp ah,4
    je Multiply
    cmp ah,5
    je Division
    ret
equalMath:
    ;introduce first variable
    mov si,math_description0
    call printString

    mov bp,0
    mov di,0
    mov bl,0
    call firstVariableKeyboard
    
    ;new line
    mov si,newLine
    call printString

    ;introduce second variable
    mov si,math_description1
    call printString

    mov bp,0
    call secondVariableKeyboard

    ;new line
    mov si,newLine
    call printString
    ;choose operator
    mov si,math_description3
    call printString

    ;new line
    mov si,newLine
    call printString

    call chooseOperator

Addition:
        ;the result
        mov si,math_description2
        call printString
        ;adding
        mov dx,0
        mov cx,0
        ;compare if var1 has one digit typed and var 2 two digits
        cmp di,2  ;var1
        je compareOneDigitTwoDigits
        ;compare if var2 has one digit typed and var 1 two digits
        cmp bl,2 ;var2
        je compareTwoDigitsOneDigit
        back3:
        ;compare if its adding 8+8 
        cmp bp,2
        je oneDigitMath
        backOneDigitMath:
        ;compare if addin 23+12
        cmp bp,4
        je twoDigitMath

        ;call the function that makes from int ascii
        call divMath
        mov bx,0
        ;print the result
        mov bp,0
        mov si,result
        ;compare to print 1 or 2 digit result
        cmp cx,2
        je ifEqualTwo
        back0:
        cmp cx,4
        je ifEqualFour
        jmp General
        oneDigitMath:
        sub word[var1],48
        sub word[var2],48

        mov dx,[var1]
        add dx,[var2]

        mov ax,dx
        mov bp,0
        mov cx,0

        jmp backOneDigitMath
    twoDigitMath:
        mov cx,0

        sub word[var1],48
        sub word[var2],48
        sub word[var1 + 2],48
        sub word[var2 + 2],48

        mov dx,[var1 + 2]
        add dx,[var2 + 2]

        cmp dx,9
        jg callDivMath

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        backFromDivMath:
        add dx,[var1] 
        add dx,[var2] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

    callDivMath:
        mov ax,dx
        mov bp,0
        call divMath
        call addOneDigit
        jmp backFromDivMath
    addOneDigit: 
        mov dx,0
        mov cx,0
        mov dx,[result + 2]
        sub dx,48
        mov cx,[result + 0]
        mov word[result + 2], cx
        ret
    compareOneDigitTwoDigits:
        cmp bl,4
        je oneDigitTwoDigits
        jmp back3
    addNewOneDigitTwoDigits:
        mov dx,0 
        mov cx,0
        mov dx,[result + 2] 
        sub dx,48
        mov cx,[result + 0] 
        mov word[result + 2], cx
        ret
    callOneDigitTwoDigits:
        mov ax,dx
        mov bp,0
        call divMath
        call addNewOneDigitTwoDigits
        jmp backFromCall
    oneDigitTwoDigits:
        mov cx,0 

        sub word[var1],48  
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov dx,[var1]
        add dx,[var2 + 2] 
        
        cmp dx,9
        jg callOneDigitTwoDigits

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        backFromCall:
        add dx,[var2] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret

    compareTwoDigitsOneDigit:
        cmp di,4
        je twoDigitsOneDigit
        jmp back3
    addNewTwoDigitsOneDigit:
        mov dx,0 
        mov cx,0
        mov dx,[result + 2] 
        sub dx,48
        mov cx,[result + 0] 
        mov word[result + 2], cx
        ret
    callTwoDigitsOneDigits:
        mov ax,dx
        mov bp,0
        call divMath
        call addNewTwoDigitsOneDigit
        jmp backFromCall1
    twoDigitsOneDigit:
        mov cx,0 

        sub word[var1],48 
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov dx,[var1 + 2]
        add dx,[var2] 
        
        cmp dx,9
        jg callTwoDigitsOneDigits

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        backFromCall1:
        add dx,[var1] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret

Subtraction:
    ;the result
    mov si,math_description2
    call printString

    ;subtract
        mov dx,0
        mov cx,0
        ;compare if var1 has one digit typed and var 2 two digits
        cmp di,2  ;var1
        je ScompareOneDigitTwoDigits
        ;compare if var2 has one digit typed and var 1 two digits
        cmp bl,2 ;var2
        je ScompareTwoDigitsOneDigit
        Sback3:
        ;compare if its adding 8+8 
        cmp bp,2
        je SoneDigitMath
        SbackOneDigitMath:
        ;compare if addin 23+12
        cmp bp,4
        je StwoDigitMath

        ;call the function that makes from int ascii
        call divMath
        mov bx,0
        ;print the result
        mov bp,0
        mov si,result
        ;compare to print 1 or 2 digit result
        cmp cx,2
        je ifEqualTwo
        Sback0:
        cmp cx,4
        je ifEqualFour
        jmp General
    SoneDigitMath:
        sub word[var1],48
        sub word[var2],48

        mov dx,[var1]
        sub dx,[var2]

        mov ax,dx
        mov bp,0
        mov cx,0

        jmp SbackOneDigitMath
    StwoDigitMath:
        mov cx,0

        sub word[var1],48
        sub word[var2],48
        sub word[var1 + 2],48
        sub word[var2 + 2],48

        mov dx,[var1 + 2]
        add dx,[var2 + 2]

        cmp dx,9
        jg ScallDivMath

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        SbackFromDivMath:
        add dx,[var1] 
        add dx,[var2] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

    ScallDivMath:
        mov ax,dx
        mov bp,0
        call divMath
        call SaddOneDigit
        jmp SbackFromDivMath
    SaddOneDigit: 
        mov dx,0
        mov cx,0
        mov dx,[result + 2]
        sub dx,48
        mov cx,[result + 0]
        mov word[result + 2], cx
        ret
    ScompareOneDigitTwoDigits:
        cmp bl,4
        je SoneDigitTwoDigits
        jmp Sback3
    SaddNewOneDigitTwoDigits:
        mov dx,0 
        mov cx,0
        mov dx,[result + 2] 
        sub dx,48
        mov cx,[result + 0] 
        mov word[result + 2], cx
        ret
    ScallOneDigitTwoDigits:
        mov ax,dx
        mov bp,0
        call divMath
        call SaddNewOneDigitTwoDigits
        jmp SbackFromCall
    SoneDigitTwoDigits:
        mov cx,0 

        sub word[var1],48  
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov dx,[var1]
        add dx,[var2 + 2] 
        
        cmp dx,9
        jg ScallOneDigitTwoDigits

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        SbackFromCall:
        add dx,[var2] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret

    ScompareTwoDigitsOneDigit:
        cmp di,4
        je StwoDigitsOneDigit
        jmp Sback3
    SaddNewTwoDigitsOneDigit:
        mov dx,0 
        mov cx,0
        mov dx,[result + 2] 
        sub dx,48
        mov cx,[result + 0] 
        mov word[result + 2], cx
        ret
    ScallTwoDigitsOneDigits:
        mov ax,dx
        mov bp,0
        call divMath
        call SaddNewTwoDigitsOneDigit
        jmp SbackFromCall1
    StwoDigitsOneDigit:
        mov cx,0 

        sub word[var1],48 
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov dx,[var1 + 2]
        add dx,[var2] 
        
        cmp dx,9
        jg ScallTwoDigitsOneDigits

        mov [result + 2],dx
        add word[result + 2],48
        mov dx,0
        SbackFromCall1:
        add dx,[var1] 
        mov word[result],dx

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret


    jmp General
Multiply:
    ;the result
    mov si,math_description2
    call printString

    ;multiply
        mov dx,0
        mov cx,0
        ;compare if var1 has one digit typed and var 2 two digits
        cmp di,2  ;var1
        je McompareOneDigitTwoDigits
        ;compare if var2 has one digit typed and var 1 two digits
        cmp bl,2 ;var2
        je McompareTwoDigitsOneDigit
        Mback3:
        ;compare if its adding 8+8 
        cmp bp,2
        je MoneDigitMath
    MbackOneDigitMath:
        
        ;call the function that makes from int ascii
        call divMath
        mov bx,0
        ;print the result
        mov bp,0
        mov si,result
        ;compare to print 1 or 2 digit result
        cmp cx,2
        je ifEqualTwo
        Mback0:
        cmp cx,4
        je ifEqualFour
        jmp General
    MoneDigitMath:
        sub word[var1],48
        sub word[var2],48

        mov ax,[var1]
        mov dx,[var2]

        mul dx

        ; mov ax,dx
        mov bp,0
        mov cx,0

        jmp MbackOneDigitMath
    
    McompareOneDigitTwoDigits:
        cmp bl,4
        je MoneDigitTwoDigits
        jmp Mback3
    MaddNewOneDigitTwoDigits:
        mov dx,0  
        mov cx,0
        mov cx,[result + 2] 
        sub cx,48
        mov dx,[result + 0]
        mov word[result + 2], dx
        mov dx,0
        ret
    McallOneDigitTwoDigits:
        mov bp,0
        call divMath
        call MaddNewOneDigitTwoDigits
        jmp MbackFromCall
    MoneDigitTwoDigits:
        mov ax,0
        mov cx,0 

        sub word[var1],48  
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov ax,[var1]
        mov dx,[var2 + 2] 

        mul dx
        
        cmp ax,9
        jg McallOneDigitTwoDigits

        mov [result + 2],ax
        add word[result + 2],48
        mov ax,0
        mov cx,0
    MbackFromCall:
        mov ax,[var2]
        mov dx,[var1]
        mul dx
        add ax,cx
        mov word[result],ax
        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret

    McompareTwoDigitsOneDigit:
        cmp di,4
        je MtwoDigitsOneDigit
        jmp Mback3
    MaddNewTwoDigitsOneDigit:
        mov dx,0 
        mov cx,0
        mov cx,[result + 2] 
        sub cx,48
        mov dx,[result + 0] 
        mov word[result + 2], dx
        ret
    McallTwoDigitsOneDigits:
        mov bp,0
        call divMath
        call MaddNewTwoDigitsOneDigit
        jmp MbackFromCall1
    MtwoDigitsOneDigit:
        mov cx,0 

        sub word[var1],48 
        sub word[var2],48  
        sub word[var1 + 2],48
        sub word[var2 + 2],48
        
        mov ax,[var1 + 2] 
        mov dx,[var2] 
        mul dx

        cmp ax,9
        jg McallTwoDigitsOneDigits

        mov [result + 2],ax
        add word[result + 2],48
        mov dx,0
        mov cx,0
        MbackFromCall1:
        mov dx,[var2] 
        mov ax,[var1]
        mul dx
        add ax,cx
        mov word[result],ax

        add word[result],48

        mov bp,0
        mov si,result
        call printStringDouble
        
        jmp General

        ret

    jmp General
Division:
    ;the result
    mov si,math_description2
    call printString

    jmp General
General:
    mov bx,0
    call clearBuffer

    ;new line
    mov si,newLine
    call printString
    
    jmp mainLoop
divMath:
    mov dx,0
    mov bx,10
    div bx
    ;covert to ascii
    add dx,48
    ;push to buffer current number
    mov word[result + bp],dx
    ;increment
    add bp,2
    add cx,2

    ;if divider is 0 exit
    cmp ax,0
    jnz divMath
    ret
clearBuffer:
    mov word[var1 + bx],0
    mov word[var2 + bx],0
    mov word[result + bx],0
    add bx,2
    cmp bx,6
    jne clearBuffer
    mov bx,0
    ret

;if restart typed
equalRestart:
    mov si, restart_description
    call printString
    waitRestartKeyboard:    
    mov ah,00h
    int 16h
    cmp ah,21
    je restartJump
    cmp ah,49
    je mainLoop
    jmp waitRestartKeyboard
restartJump:
    int 19h
;if ghost typed
equalGhost:
    mov si, ghost_description0
    call printString
    
    mov si, ghost_description1
    call printString
        
    mov si, ghost_description2
    call printString
        
    mov si, ghost_description3
    call printString
        
    mov si, ghost_description4
    call printString
        
    mov si, ghost_description5
    call printString
        
    mov si, ghost_description6
    call printString

    jmp mainLoop
;if clear typed
equalClear:
    mov ah,00h
    mov al,3
    int 10h

    jmp mainLoop
;if about typed
equalAbout:
    mov si,about_description0
    call printString

    mov si, about_description1
    call printString

    call read_and_display_rtc_time
    
    
    jmp mainLoop
read_and_display_rtc_time:
    mov si,2
    mov dx,[storeTime1]
    mov bp,0
    call print_hex

    mov ah,0eh
    mov al,47
    int 10h
    
    mov si,1
    mov dx,[storeTime3]
    call print_hex

    mov si,alotOfSpace
    call printString

    mov si,2
    mov dx,[storeTime2]
    mov bp,8
    call print_hex

    mov si,newLine
    call printString
    jmp mainLoop
    

; Prints the value of DX as hex.
print_hex:
  pusha             ; save the register values to the stack for later

  mov cx,4          ; Start the counter: we want to print 4 characters
                    ; 4 bits per char, so we're printing a total of 16 bits

char_loop:
  dec cx            ; Decrement the counter

  mov ax,dx         ; copy bx into ax so we can mask it for the last chars
  shr dx,4          ; shift bx 4 bits to the right
  and ax,0xf        ; mask ah to get the last 4 bits

  mov bx, HEX_OUT   ; set bx to the memory address of our string
  add bx, 2         ; skip the '0x'
  add bx, cx        ; add the current counter to the address

  cmp ax,0xa        ; Check to see if it's a letter or number
  jl set_letter     ; If it's a number, go straight to setting the value
  add al, 0x27      ; If it's a letter, add 0x27, and plus 0x30 down below
                    ; ASCII letters start 0x61 for "a" characters after 
                    ; decimal numbers. We need to cover that distance. 
  jl set_letter

set_letter:
  add al, 0x30      ; For and ASCII number, add 0x30
  mov byte [bx],al  ; Add the value of the byte to the char at bx

  cmp cx,0          ; check the counter, compare with 0
  je print_hex_done ; if the counter is 0, finish
  jmp char_loop     ; otherwise, loop again

print_hex_done:
  mov bx, HEX_OUT   ; print the string pointed to by bx
  call print_string

  popa              ; pop the initial register values back from the stack
  ret               ; return the function

HEX_OUT: db '0x0000', 0
print_string:     ; Push registers onto the stack
  pusha
  je string_loop

string_loop:
    cmp si,1
    je al4
    cmp si,2
    je al2
    alback:
    cmp al, 0       ; Compare the value in al to 0 (check for null terminator)
    jne print_char  ; If it's not null, print the character at al
                  ; Otherwise the string is done, and the function is ending
    popa            ; Pop all the registers back onto the stack
    ret             ; return execution to where we were

print_char:
    mov ah, 0x0e    ; Linefeed printing
    int 0x10        ; Print character
    add bx, 1       ; Shift bx to the next character
    add bp,1
    cmp bp,2
    je displayBar
    cmp bp,10
    je display_colon
    jmp string_loop ; go back to the beginning of our loop

display_colon:
    mov ah,0eh
    mov al,58
    int 10h
    add bp,1
    jmp string_loop
displayBar:
    cmp si,1
    je string_loop
    mov ah,0eh
    mov al,47
    int 10h
    jmp string_loop
al4:
    mov al, [bx + 4]    ; Set al to the value at bx
    jmp alback
al2:
    mov al, [bx + 2]    ; Set al to the value at bx
    jmp alback

    
;if draw typed
equalDraw:
    jmp draw
;if exit typed  
equalShutdown:
    ; mov bx,0
    mov si, goodbye           ; печатаем прощание
    call printString

    jmp $  
;if nothing typed
equalEmptySpace:
    ;mov bx,0
    jmp mainLoop
;if help was written
equalHelp:
    ;mov bx,0
	mov si, help_description0
    call printString

    mov si, help_description1
    call printString
     
    mov si, help_description2
    call printString

    mov si, help_description3
    call printString

    mov si, help_description4
    call printString

    mov si, help_description5
    call printString

    mov si, help_description6
    call printString

    jmp mainLoop
;if no valid command written
equalNothing:
    ; mov bx,0
	mov si, wrong_command
    call printString

    jmp mainLoop
;backspace functionality
upCursor:                          ;when backspace and x coord is 0 line up
    sub dh,1                ;line up
    add dl,80               ;x coord set to last char
    call setCursor
    jmp backspacePressed
    ret
setCursor:               ;set the cursor
    mov ah,0x02
    int 0x10
    ret
newUpCursor:                  ;delete the 80th char from the line and place the cursor on 79th
    sub dl,1             ;position cursor on 79th column
    call setCursor  
    jmp backspacePressed
    ret
;backsapce pressed
backspacePressed:
	cmp bx, 0                ; if backspace pressed and input is 0
    je inputFieldButtons      ; do nothing for not deleting the prompt_message

    mov ah,0x03                ;cursor
    mov bh,0
    int 0x10
    cmp dl,0                 ;output cursor on x coord
    je upCursor

    mov ah, 0x0e                ;tty mode         

    int 0x10                    ; print backspace and the cursor moves back but char not erased

    mov al, ' '               ; print space to erase the char and cursor moved forward
    int 0x10                

    mov al, 0x8               ; again backspace to go to initial position
    int 0x10                    

    dec bx
    mov byte [input+bx], 0    ; erase from input the last char

    cmp dl,80                 ;delete char from 80th column and place cursor on 79th
    je newUpCursor

    jmp inputFieldButtons   
;draw command    
draw:
        ;set video mode to 640x200
        mov ah,00h
        mov al,0eh
        int 10h
        ;print a pixel
        jmp printpixel
    downObject:
        mov ah,0ch
        int 10h
        inc cx
        inc dx
        cmp dx, 120
        jne downObject
        ret
    rightObject:
        mov ah,0ch
        int 10h
        inc cx
        cmp cx, 320
        jne rightObject
        ret
    upObject:
        mov ah,0ch
        int 10h
        dec dx
        inc cx
        cmp dx, 40
        jne upObject
        ret
    leftBitObject:
        mov ah,0ch
        int 10h
        dec cx
        cmp cx, 370
        jne leftBitObject
        ret
    downBitObject:
        mov ah,0ch
        int 10h
        inc dx
        dec cx
        cmp dx, 100
        jne downBitObject
        ret
    leftleftBitObject:
        mov ah,0ch
        int 10h
        dec cx
        cmp cx, 290
        jne leftleftBitObject
        ret
    upBitObject:
        mov ah,0ch
        int 10h
        dec dx
        dec cx
        cmp dx, 40
        jne upBitObject
        ret
    leftLeftLeftBitObject:
        mov ah,0ch
        int 10h
        dec cx
        cmp cx, 200
        jne leftLeftLeftBitObject
        ret
    logoSquareDown:
        mov ah,0ch
        int 10h
        inc dx
        cmp dx, 130
        jne logoSquareDown
        ret
    logoSquareRight:
        mov ah,0ch
        int 10h
        inc cx
        cmp cx, 410
        jne logoSquareRight
        ret
    logoSquareUp:
        mov ah,0ch
        int 10h
        dec dx
        cmp dx, 30
        jne logoSquareUp
        ret
    logoSquareLeft:
        mov ah,0ch
        int 10h
        dec cx
        cmp cx, 190
        jne logoSquareLeft
        ret
    clearScreen:
        mov ah,00h
        mov al,0eh
        int 10h
        jmp continue
    triangleDown:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        inc dx
        ;cmp cx,540
        cmp dx,140
        jne triangleDown
        ret
    triangleLeft:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,460
        jne triangleLeft
        ret
    triangleUp:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        dec dx
        cmp dx,100
        jne triangleUp
        ret
    headRight:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,520
        jne headRight
        ret
    headUp:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        cmp dx,80
        jne headUp
        ret
    headLeft:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,480
        jne headLeft
        ret
    headDown:
        mov ah,0ch
        mov al,9
        int 10h
        inc dx
        cmp dx,100
        jne headDown
        ret
    eyeUp1:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        cmp dx,85
        jne eyeUp1
        ret
    eyeRight1:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,495
        jne eyeRight1
        ret
    eyeDown1:
        mov ah,0ch
        mov al,9
        int 10h
        inc dx
        cmp dx,90
        jne eyeDown1
        ret
    eyeLeft1:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,490
        jne eyeLeft1
        ret
    eyeRight2:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,510
        jne eyeRight2
        ret
    eyeLeft2:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,505
        jne eyeLeft2
        ret
    footDown:
        mov ah,0ch
        mov al,9
        int 10h
        inc dx
        cmp dx,150
        jne footDown
        ret
    footRight1:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,490
        jne footRight1
        ret
    footRight2:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,530
        jne footRight2
        ret
    footUp:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        cmp dx,140
        jne footUp
        ret
    handDown:
        mov ah,0ch
        mov al,9
        int 10h
        inc dx
        cmp dx,115
        jne handDown
        ret
    handRight1:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,535
        jne handRight1
        ret
    handUp:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        cmp dx,110
        jne handUp
        ret
    handLeft1:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,510
        jne handLeft1
        ret
    handLeft2:
        mov ah,0ch
        mov al,9
        int 10h
        dec cx
        cmp cx,465
        jne handLeft2
        ret
    handRight2:
        mov ah,0ch
        mov al,9
        int 10h
        inc cx
        cmp cx,490
        jne handRight2
        ret
    squareRight:
        mov ah,0ch
        int 10h
        inc cx
        cmp cx,si
        jne squareRight
        ret
    squareUp:
        mov ah,0ch
        int 10h
        dec dx
        cmp dx,di
        jne squareUp
        ret
    squareLeft:
        mov ah,0ch
        int 10h
        dec cx
        cmp cx,si
        jne squareLeft
        ret
    squareDown:
        mov ah,0ch
        int 10h
        inc dx
        cmp dx,di
        jne squareDown
        ret
    newSquareRight:
        call deleteSquare
        mov al,14
        add si,5
        mov cx,si
        add si,10
        call squareRight
        sub di,5
        call squareUp
        sub si,10
        call squareLeft
        add di,5
        call squareDown 
        jmp waitKeyboard
    newSquareLeft:
        call deleteSquare
        mov al,14
        sub si,5
        mov cx,si
        add si,10
        call squareRight
        sub di,5
        call squareUp
        sub si,10
        call squareLeft
        add di,5
        call squareDown
        jmp waitKeyboard
    newSquareDown:
        call deleteSquare
        mov al,14
        add di,5
        mov dx,di
        mov cx,si
        add si,10
        call squareRight
        sub di,5
        call squareUp
        sub si,10
        call squareLeft
        add di,5
        call squareDown
        jmp waitKeyboard
    newSquareUp:
        call deleteSquare
        mov al,14
        sub di,5
        mov dx,di
        mov cx,si
        add si,10
        call squareRight
        sub di,5
        call squareUp
        sub si,10
        call squareLeft
        add di,5
        call squareDown
        jmp waitKeyboard
    deleteSquare:
        mov al,0
        add si,10
        call squareRight
        sub di,5
        call squareUp
        sub si,10
        call squareLeft
        add di,5
        call squareDown
        ret
    gradientPixel:
        mov al,1
        cmp si,1
        je goRightSquare
        cmp si,2
        je goDownSquare
        cmp si,3
        je goLeftSquare
        cmp si,4
        je goUpSquare
        ret
    incrementColor:
        mov di,0
        inc al
        cmp si,1
        je goRightSquare
        cmp si,2
        je goDownSquare
        cmp si,3
        je goLeftSquare
        cmp si,4
        je goUpSquare
    goRightSquare:
        mov si,1
        mov ah,0ch
        cmp al,15
        je gradientPixel
        int 10h
        cmp di,5
        je incrementColor
        inc di
        inc cx
        cmp cx,80
        jne goRightSquare
        ret
    goDownSquare:
        mov si,2
        mov ah,0ch
        cmp al,15
        je gradientPixel
        int 10h
        cmp di,5
        je incrementColor
        inc di
        inc dx
        cmp dx,80
        jne goDownSquare
        ret
    goLeftSquare:
        mov si,3
        mov ah,0ch
        cmp al,15
        je gradientPixel
        int 10h
        cmp di,5
        je incrementColor
        inc di
        dec cx
        cmp cx,40
        jne goLeftSquare
        ret
    goUpSquare:
        mov si,4
        mov ah,0ch
        cmp al,15
        je gradientPixel
        int 10h
        cmp di,5
        je incrementColor
        inc di
        dec dx
        cmp dx,40
        jne goUpSquare
        ret
    simpleTriangleDown:
        mov ah,0ch
        mov al,9
        int 10h
        add dx,2
        inc cx
        cmp cx,200
        cmp dx,120
        jne simpleTriangleDown
        ret
    simpleTriangleLeft:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        sub cx,2
        cmp cx,140
        cmp dx,70
        jne simpleTriangleLeft
        ret
    simpleTriangleUp:
        mov ah,0ch
        mov al,9
        int 10h
        dec dx
        add cx,2
        cmp cx,200
        cmp dx,40
        jne simpleTriangleUp
        ret
printpixel:
    ;logo
    mov al,12
    mov cx,200
    mov dx,40
    call downObject
    call rightObject
    call upObject
    call leftBitObject
    call downBitObject
    call leftleftBitObject
    call upBitObject
    call leftLeftLeftBitObject
    mov cx,190
    mov dx,30
    mov al,14
    call logoSquareDown
    call logoSquareRight
    call logoSquareUp
    call logoSquareLeft
    call waitKeyboard
    continue:
    ;simple objects
    mov cx,40 ;set inital square column
    mov dx,40 ;set initial square row
    mov al,1
    mov di,0
    call goRightSquare
    call goDownSquare
    call goLeftSquare
    call goUpSquare
    mov cx,160 ;set initial triangle column
    mov dx,40 ;set initial triangle row
    call simpleTriangleDown
    call simpleTriangleLeft
    call simpleTriangleUp
    ;robot
    mov cx,500 ;set inital square column
    mov dx,100 ;set initial square row
    call triangleDown
    call triangleLeft
    call triangleUp
    mov cx,480 ;coord for head
    mov dx,100 
    call headRight
    call headUp
    call headLeft
    call headDown
    mov cx,490 ;coord for eye1
    mov dx,90
    call eyeUp1
    call eyeRight1
    call eyeDown1
    call eyeLeft1
    mov cx,505 ;coord for eye2
    call eyeUp1
    call eyeRight2
    call eyeDown1
    call eyeLeft2
    mov cx,470 ;coord foot1
    mov dx,140
    call footDown
    call footRight1
    call footUp
    mov cx,510 ;coord foot2
    call footDown
    call footRight2
    call footUp
    mov cx,510 ;coord hand1
    mov dx,110
    call handDown
    call handRight1
    call handUp
    call handLeft1
    mov cx,490 ;coord hand2
    call handDown
    call handLeft2
    call handUp
    call handRight2
    ;animation
    mov al,14
    mov cx,30 ;set initial row
    mov dx,20 ;set initial column
    mov si,cx
    mov di,dx

    add si,10
    call squareRight
    sub di,5
    call squareUp
    sub si,10
    call squareLeft
    add di,5
    call squareDown
    call waitKeyboard
    ret
waitKeyboard:
    mov ah,00h
    int 16h
    cmp ah,12h
    je exitDrawCommand
    cmp ah,4dh
    je newSquareRight
    cmp ah,4bh
    je newSquareLeft
    cmp ah,50h
    je newSquareDown
    cmp ah,48h
    je newSquareUp
    cmp ah,57
    je clearScreen
    jmp waitKeyboard
exitDrawCommand:
    mov ah,00h
    mov al,3
    int 10h
    jmp mainLoop


greetings: db " Type 'help' to access the documentation. ",0x0d, 0xa, 0xa, 0
prompt_message: db "#>",0
newLine: db 0x0d, 0xa, 0
input: times 64 db 0 
var1: times 64 db 0 
var2: times 64 db 0 
time: times 64 db 0
result:  times 64 db 0 
storeTime1: times 10 db 0
storeTime2: times 10 db 0
storeTime3: times 10 db 0
goodbye: db "Goodbye! Have a nice day.", 0x0d, 0xa, 0
help_command: db "help", 0
empty_space: db "",0
alotOfSpace: db "  ", 0
shutdown_command: db "shutdown", 0
draw_command: db "draw", 0
about_command: db "about", 0
wrong_command: db "Wrong command!", 0x0d, 0xa, 0
clear_command: db "clear", 0
ghost_command: db "ghost", 0
restart_command: db "restart", 0
math_command: db "math", 0
help_description0: db " These are the shell commands you can use:", 0x0d, 0xa, 0
help_description1: db "   Type 'draw' to show pixel images. To return back press 'e'. Press 'space' during logo ", 0x0d, 0xa, 0
help_description2: db "   Type 'about' for information about the Operating System.", 0x0d, 0xa, 0
help_description3: db "   Type 'shutdown' to shutdown the Operating System.", 0x0d, 0xa, 0
help_description4: db "   Type 'clear' to clear the screen.", 0x0d, 0xa, 0
help_description5: db "   Type 'ghost' to see a ghost on the screen.", 0x0d, 0xa, 0
help_description6: db "   Type 'restart' to restart the Operating System.", 0x0d, 0xa, 0
about_description0: db "Codename: :) Version 0.1", 0x0d, 0xa, 0
about_description1: db "Hi! My name is Vadim and this is my OS. If you find bugs please contact me.     Thank you!", 0x0d, 0xa, 0
ghost_description0: db " This is a ghost! Mhuuuuu! ", 0x0d, 0xa, 0
ghost_description1: db "        _________", 0x0d, 0xa, 0
ghost_description2: db "       (  O   o )", 0x0d, 0xa, 0
ghost_description3: db "       /    0    \", 0x0d, 0xa, 0
ghost_description4: db "      /           \", 0x0d, 0xa, 0
ghost_description5: db "     /             \", 0x0d, 0xa, 0
ghost_description6: db "    ~~~~~~~~~~~~~~~~~", 0x0d, 0xa, 0
restart_description: db "The OS will now restart. Press 'y' to continue and 'n' if you changed your mind.", 0
math_description0: db "Introduce the first variable", 0x0d, 0xa, 0
math_description1: db "Introduce the second variable", 0x0d, 0xa, 0
math_description2: db "The result is ", 0x0d, 0xa, 0
math_description3: db "Choose the operation 1.Add 2.Subtract 3.Multiply 4.Divide"







