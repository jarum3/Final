;; [bp+4] should hold passed int, after function call and base pointer are pushed onto stack

include ioFunc.asm
; IOFunc -> intFunc -> arrFunc -> final

proc square
; Takes value on stack as input, returns value on stack squared in ax, or -1 if value is too large.
; Prologue
push bp ; Save base pointer
mov bp, sp ; Move initial stack pointer into base pointer
; Main
mov ax, [bp+4] ; Grab first argument into ax
cmp ax, 181 ; Approximate square root of max value
jg squareError ; Return -1
imul ax ; Signed multiply ax by ax
jmp squareExiter
squareError:
  mov ax, -1
  ; Fall into next label
squareExiter:
  mov sp, bp ; Restore initial stack pointer
  pop bp ; Pop base pointer from stack
  ret
square endp

proc necessaryBits
;; Takes value on stack as input, returns the number of bits necessary to represent that number in ax.
;; You can add up powers of 2, or just trim off leading zeros from the actual binary value
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
necessaryBits endp

proc isPrime
;; Takes value on stack as input, returns a boolean (1/0) of whether that number is a prime number.
;; You can simply loop through every number up to x/2, divide it, check the remainder if its zero.
;; Return true if x divides evenly (0 remainder) into any number that's not 1 or x.
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
isPrime endp

proc perfectSquare
;; Takes value on stack as input, returns a boolean (1/0) of whether or not that number is a perfect square
;; You can simply loop through every number up to sqrt(x), square it (push i, call square, pop i into a different register)
;; If it squares into x, x is a perfect square.
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
perfectSquare endp

proc isNegative
;; Returns a boolean on whether or not a value on the stack is negative
  ; Prologue
  push bp
  mov bp, sp
  ; Main
  mov ax, [bp+4] ; Move argument into ax
  or ax, ax ; Sets SF to sign of AX
  jge negativeFalse ;; Checks sign flag, jumps if its not set.
  mov ax, 1 ; Return 1 if sign bit is true
  jmp negativeexiter ; Return
  negativeFalse:
    xor ax, ax ;; Return 0 if sign bit is false
    ; Fall into next label
  negativeExiter:
  ; Epilogue
  mov sp, bp
  pop  bp
  ret
isNegative endp

proc isEven
;; Returns a boolean on whether or not a value on the stack is even
;; This is equal to the least-significant bit
;; Does not use any registers it doesn't return.
  ; Prologue
  push bp
  mov bp, sp
  ; Main
  mov ax, [bp+4] ; Move argument into ax
  and ax, 1b ; Set all bits except for least-significant to 0
  ; Epilogue
  mov sp, bp
  pop  bp
  ret
isEven endp

proc printBinary
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
printBinary endp

proc printOctal
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
printOctal endp

proc printHex
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
printHex endp

printDec proc
; Prints value on stack as a signed decimal integer
  ; Prologue
  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  push dx
  ; Main
  mov ax, [bp+4]
  or ax, ax ; Sets SF to sign bit and ZF to ax==0
  jge @prtPositive ; If ax isn't negative, skip the negation
      push ax ; Save ax temporarily
      mov dl, '-'
      mov ah, 2
      int 21h ; Prints minus sign
      pop ax ; Restores ax
      neg ax ; Flip ax for printing
  @prtPositive:
  ;get decimal digits
    xor cx, cx ; Clear cx so that it can increment number of digits
    mov bx, 10D ; Put 10 into bx for division
  @DigitLoop:
    div bx ; AX = quotient, DX = remainder
    push dx ; Save remainder to stack (Remainder will be least-significant digit)
    inc cx ; Increase number of digits for each division
  ;until
    or ax, ax ; Sets zero flag if ax==0
    jnz @DigitLoop ; If there's still quotient left, keep dividing it and pushing it to stack
  ;convert digits to characters and print
    mov ah, 2 ; DOS print character
  @PrintLoop:
    pop dx ; Grabs first digit (Top of stack)
    or dl, 30h ; Bitmask to convert to char
    int 21h ; DOS interrupt to print char
    loop @PrintLoop ; Loops cx times, cx has digit count, loops over each digit.
  ; Epilogue
  pop dx
  pop cx
  pop bx
  pop ax
  mov sp, bp
  pop bp
  ret
printDec endp

;; No arguments, returns user input as signed int in ax
getDec proc
  ; Prologue
  push bp
  mov bp, sp
  push bx
  push cx
  push dx
  ; Main
  @startGet:
    xor bx, bx ; Clear bx to hold total
    xor cx, cx ; Clear cx to hold sign
    ; Read a char
    mov ah, 1
    int 21h ; DOS returns char in AL
    ; Grab sign character
    cmp al, '-' ; If minus sign, parse as negative int
    je @getNegative
    cmp al, '+'
    je @getPositive ; If +, start getting integers
    jmp @getDigit ; Process digits
    @getNegative:
      mov cx, 1 ; Set sign flag, then start processing as positive
    @getPositive:
      int 21h ; Read a char, don't save sign char
    @getDigit:
      ;if character is between '0' and '9'
      cmp al, '0'
      jl @notDig ; If it's less than 0 ascii, it's not a char
      cmp al, '9'
      jg @notDig
      and ax, 000Fh ; Bit mask to convert ascii to int    
      push ax ; Save value
      mov ax, 10 ; Multiply bx by 10
      mul bx ; bx*10 -> ax (bx shifted over one to the left in decimal)
      pop bx ; pop prev ax into bx
      add bx, ax ; Combine total (Concatenate new digit onto end of bx)
      ; Read char
      mov ah, 1
      int 21h
      cmp al, 0Dh ; Carriage return should end loop
      jne @getDigit ; Loop until carriage return is entered
      mov ax, bx ; Store total into ax
      or cx, cx ; Checks sign flag into zero flag
      jz @stopGetting ; Skip negation if positive
      neg ax ; Invert AX
      @stopGetting:
        ; Epilogue
        pop dx
        pop cx
        pop bx
        mov sp, bp
        pop bp
        ret
      @notDig: ; Print newline, re-loop
        mov ah, 2
        mov dl, 0Dh
        int 21h
        mov dl, 0Ah
        int 21h
        jmp @startGet
getDec endp