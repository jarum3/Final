.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
true db "True", '$'
false db "False", '$'
pauser db 0Ah, 0Dh, "Press any key...", 0Ah, 0Dh, '$'
.code ; Code segment

;; Clears the screen
clrScr macro

  push ax ; Saving ax
  mov ax, 0002h ; Clearing screen in BIOS
  int 10h
  pop ax ; Restoring ax
endm

pause macro

  prtStr pauser
  getChar
endm

; Bios interrupt for changing color, takes one byte
chgColor macro color
	
  ; Prologue
  push ax
  push bx
  push cx
  push dx
  ; Main
  mov ah,06h
	xor al, al
	mov bh,color
	xor cx, cx
	mov dx,184fh		; paint to row 23 column 79
	int 10h
  ; Epilogue
  pop dx
  pop cx
  pop bx
  pop ax
endm

; Sound I/O interface
sound macro freq,duration
  ; Prologue
  push ax
  push cx
  push dx
  ; Main
	mov al, 182         ; Prepare the speaker for the
	out 43h, al         ; note.
	mov ax,freq			    ; Set frequency to go out.
	out 42h, al         ; Output low byte.
	mov al, ah          ; Output high byte.
	out 42h, al 
	in  al, 61h         ; Turn on note (get value from port 61h).
	or  al, 00000011b   ; Set bits 1 and 0.
	out 61h, al         ; Send new value.
	mov cx,duration		  ; Pause for duration of note.
	mov dx,0fh
	mov ah,86h			    ; CX:DX = how long pause is? I'm not sure exactly how it works but it's working
	int 15h				      ; Pause for duration of note.
	in  al, 61h         ; Turn off note (get value from
                      ;  port 61h).
	and al, 11111100b   ; Reset bits 1 and 0.
	out 61h, al         ; Send new value.
	
	mov cx,01h			;Pause to give the notes some separation
	mov dx,08h
	mov ah,86h
	int 15h
  ; Epilogue
  pop dx
  pop cx
  pop ax
endm

;; Gets one char from user, puts it in ax
getChar macro

  ; Main
  mov ah, 01h ; Setting for interrupt
  int 21h ; Moves input char to AL
  xor ah, ah ; Clears ah, ax = al
endm

; Prints a single one-byte decimal digit to screen
prtDig macro dig
  
  ; Prologue
  push ax
  push dx
  ; Main
  mov dl, dig
  or dl, 00110000b ; Mask to convert to ASCII
  mov ah, 02h
  int 21h
  ; Epilogue
  pop dx
  pop ax
endm


; Prints a single one-byte char to screen
prtChar macro char
  
  ; Prologue
  push ax
  push dx
  ; Main
  mov dl, char
  mov ah, 02h
  int 21h
  ; Epilogue
  pop dx
  pop ax
endm

;; Prints a string to screen, takes a null-terminated string as input, returns nothing.
prtStr macro str

  ; Prologue
  push ax ; Move ax at entry on stack
  push dx ; Move dx at entry on stack
  ; Main
  mov dx, offset str ; Move the passed argument memory address into dx
  mov ah, 9 ; set int 21h to print $-terminated string from dx
  int 21h ; System interrupt, prints $-terminated string from memory offset at dx in segment ds
  ; Epilogue
  pop dx ; Return state of dx
  pop ax ; Return state of ax
endm

;; Prints a single one-byte hexadecimal digit to screen from stack
prtHexDig proc
; Prologue
push bp
mov bp, sp
push ax
push dx
; Main
mov ax, [bp+4]
cmp al, 9
jle normalDig
add al, 37h ; Convert to ascii letters
prtChar al
jmp hexdigitend
normalDig:
prtDig al
; Epilogue
hexDigitEnd:
pop dx
pop ax
mov sp, bp
pop bp
ret
prtHexDig endp

;; Simply prints a comma and then a space, useful for array formatting
prtComma macro

  ; Prologue
  push ax
  push dx
  ; Main
  mov ah, 02h ; Print char DOS
  mov dl, ','
  int 21h ; Prints comma
  mov dl, ' '
  int 21h ; Prints space
  ; Epilogue
  pop dx
  pop ax
endm

prtBool proc

  ; Prologue
  push bp
  mov bp, sp
  push ax
  ; Main
  mov ax, [bp+4]
  or ax, ax
  jz boolFalse
  prtStr true
  jmp boolExit
  boolFalse:
  prtStr false
  boolExit:
  ; Epilogue
  pop ax
  mov sp, bp
  pop bp
  ret
prtBool endp

;; Exits the program, takes no arguments, returns nothing
exit proc 
  mov ax, 4c00h ; Set int 21h to terminate program with exit code 00 (All good)
  int 21h  ; System interrupt to terminate program
  ret ; Return (This is just because it feels nicer)
exit endp