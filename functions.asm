.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
  elemFetch db 0Ah, 0Dh, "Please enter a value: ", '$'
  arr dw 5 dup(?)
  arrLen dw 5
  input db 6 dup(?)
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

;; Takes an unsigned integer argument on the stack [bp+4], prints to screen as decimal, returns nothing. Uses an empty $-terminated string saved in result
;; This is hideously over-engineered for this specific task, but I knew I'd need something like this soon enough so I wanted to try to build it myself.
printNum proc
  ; Prologue, saving values of registers to stack and saving stack pointer
  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  push dx
  ; Main
  xor cx, cx ; Initialize count as 0
  mov ax, [bp+4]
  cmp ax, 0 ; If passed argument is zero, print zero 
    ; (Since the loop relies on the ax value being 0 as an end-state, 
    ; it will start to grab other stack values if it starts at 0)
  je zero
  calculate:
    xor dx, dx ; Clear dx
    cmp ax, 0 ; If ax (Quotient) is 0, start printing
    je printer
    mov bx, 10 ; Set bx to 10 for division
    div bx ; Move the last digit to dx, ax stores rest of number
    push dx ; Push last digit to stack
    inc cx ; Increment digit count
    jmp calculate
  printer:
    pop dx ; Pop digit to dx
    add dx, 48 ; Convert dx from digit to ASCII
    ; Print character
    mov ah,02h
    int 21h
    loop printer ; Loop printer with next digit until all digits are processed
  epilogue:
    ; Restore registers and stack
    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp
    ret
  zero:
    ; Print zero, skip arithmetic 
    mov ah, 02h
    mov dx, 48
    int 21h
    jmp epilogue
printNum endp

;; Takes no arguments, gets buffered string from user defined in input, saves to input.
;; Returns ax with that string interpreted as decimal integer
getInputNum proc
  push bp
  mov bp, sp
  push dx
  push si
  ; Main
  mov ah, 0ah ; Interrupt handler for buffered input
  mov dx, offset input ; Moves memory address to buffer to hold input for interrupt
  int 21h ; Takes input from user
  mov si, offset input ; Moves SI to input buffer to pass to string2number
  call string2number ; Converts taken string to integer
  ; Epilogue
  pop si
  pop dx
  mov sp, bp
  pop bp
  ret
getInputNum endp

;; Takes input of offset of filled buffer in SI, outputs number stored in ax
proc string2number
; Prologue
push bp
mov bp, sp
push bp
push cx
push bx
; Main preparation
inc  si ; Moves offset to the number of characters entered
mov  cl, [si] ; Moves number of characters entered into cl
xor ch, ch ; Clear CH, cx = cl = number of characters
add  si, cx ; Move offset by number of characters, si points to least significant bit
xor  bx, bx ; Clear bx
mov  bp, 1 ; Digit place value.
; Start converting string.
repeat:
    push cx ; Loop counter, holds number of characters with times looped subtracted
    ;Convert character.
    mov  al, [si] ; Move current character into al.
    sub  al, 48 ; Convert digit to ascii.
    mov  ah, 0 ; Clear ah, ax = al = current character.
    xor dx, dx ; Clears dx so 00:ax points to ax
    mul  bp ; ax multiplied by current place value -> dx:ax
    add  bx, ax ; Add result to bx
    ; Multiply place value by 10 in bp
    mov ax, bp ; Duplicate initial place value
    mov cl, 3 ; cl needs to hold shift value
    shl bp, cl ; Shift 3 (Multiply by 8)
    shl ax, 1 ; Shift ax 1 (Multiply by 2)
    add bp, ax ; Add bp*8 + bp*2 = bp * 10
    ; Loop ending
    dec  si ; Move pointer to next-highest digit
    pop cx ; Restore cx from beginning of loop
    loop repeat ; Decrements cx (Which holds number of characters), then loops unless cx is 0.
  ; Epilogue
  mov ax, bx ; Preparing return value
  ; Restoring memory
  pop bx
  pop cx
  pop bp
  mov sp, bp
  pop bp
  ret
string2number endp

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

;; [bp+4]: Number of elements in array (length)
;; [bp+6]: Address of int array
;; Returns ax = Largest Value and bx = Index of largest value
;; Picks first value if duplicates exist, negative numbers are less than 0
lrgArr proc
  ; Prologue
  push bp
  mov bp, sp
  ; Don't need to push ax or bx, they need to be destroyed because they're returning values
  push cx
  push dx
  ; Main
  mov cx, [bp+4] ; Move array length into cx to loop over it
  mov bx, [bp+6] ; Move base pointer for array into bx
  mov ax, [bx] ; Make first element by default greatest
  xor dx, dx ; Set index of greatest to 0, to point to the first element
  grtElem:
    cmp [bx], ax
    jle notLarger ; Skip saving value if it's not greater
    mov ax, [bx] ; If current elem is greater than ax, move it into ax
    ; cx holds len-index, index = len-cx
    mov dx, [bp+4] ; dx = length
    sub dx, cx ; dx = index = len-cx, dx will always be greater than cx
    notLarger:
      add bx, 2 ; Size of word in bytes
      loop grtElem
  grtReturning:
    mov bx, dx ; move index into bx, ax already holds largest value
  ; Epilogue
  pop dx
  pop cx
  mov sp, bp
  pop bp
  ret
lrgArr endp

;; [bp+4]: Number of elements in array (length)
;; [bp+6]: Address of int array
;; Returns ax = Largest Value and bx = Index of largest value
;; Picks first value if duplicates exist, negative numbers are less than 0
smlArr proc
  ; Prologue
  push bp
  mov bp, sp
  ; Don't need to push ax or bx, they need to be destroyed because they're returning values
  push cx
  push dx
  ; Main
  mov cx, [bp+4] ; Move array length into cx to loop over it
  mov bx, [bp+6] ; Move base pointer for array into bx
  mov ax, [bx] ; Make first element by default greatest
  xor dx, dx ; Set index of greatest to 0, to point to the first element
  lesElem:
    cmp [bx], ax
    jge notSmaller ; Skip saving value if it's not less
    mov ax, [bx] ; If current elem is less than ax, move it into ax
    ; cx holds len-index, index = len-cx
    mov dx, [bp+4] ; dx = length
    sub dx, cx ; dx becomes index = len-cx = dx-cx
    notSmaller:
      add bx, 2 ; Size of word in bytes
      loop lesElem
  lesReturning:
    mov bx, dx ; move index into bx, ax already holds largest value
  ; Epilogue
  pop dx
  pop cx
  mov sp, bp
  pop bp
  ret
smlArr endp

;; Exits the program, takes no arguments, returns nothing
exit proc 
  mov ax, 4c00h ; Set int 21h to terminate program with exit code 00 (All good)
  int 21h  ; System interrupt to terminate program
  ret ; Return (This is just because it feels nicer)
exit endp

end main