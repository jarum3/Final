.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
  elemFetch db 0Ah, 0Dh, "Please enter a value: ", '$'
  arr dw 5 dup(?)
  arrLen dw 5
.code ; Code segment

extrn outdec: proc ; Prints a decimal value in AX as a signed integer, returns nothing
extrn indec: proc ; Gets a signed decimal value from the user, stores in AX

;; Clears the screen
clrScr macro

  push ax ; Saving ax
  mov ax, 0002h ; Clearing screen in BIOS
  int 10h
  pop ax ; Restoring ax
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

;; [bp+4]: Number of elements in array (length)
;; [bp+6]: Address of int array
;; Modifies no registers, modifies passed array with user input ints
arrInput proc
  ; Prologue
  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  ; Main
  mov cx, [bp+4] ; Move array length into cx to loop over it
  mov bx, [bp+6] ; Move base pointer for array into bx
  getElem:
    prtStr elemFetch ; Message to user to input a value
    call indec ; Grabs number as 16-bit signed int
    mov [bx], ax ; Moves number into current position in array
    add bx, 2 ; Advances array pointer by 2 bytes (1 word)
    loop getElem ; Repeat arrLen times
  ; Epilogue
  pop cx
  pop bx
  pop ax
  mov sp, bp
  pop bp
  ret
arrInput endp

;; [bp+4]: Number of elements in array (length)
;; [bp+6]: Address of int array
;; Modifies no memory
prtArr proc
  ; Prologue
  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  ; Main
  mov cx, [bp+4] ; Move array length into cx to loop over it
  mov bx, [bp+6] ; Move base pointer for array into bx
  printElem:
    mov ax, [bx] ; Moves current pointed-to value into ax to print
    call outdec ; Print value in ax
    add bx, 2 ; Advance array pointer two bytes (one word)
    cmp cx, 1
    je lastElem ; Only print a comma for formatting if we aren't on the last element (cx=1)
    prtComma
    lastElem: ; Skips past printing a comma on last element
      loop printElem
  ; Epilogue
  pop cx
  pop bx
  pop ax
  mov sp, bp
  pop bp
  ret
prtArr endp


end main