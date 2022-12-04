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
;; Include the sign bit regardless of the number's sign, 1 should be 01 and -1 should be 11, 3 should be 011.
;; You can just store the sign bit, negate, process as positive, then add the sign bit back on at the end.
; Prologue
push bp
mov bp, sp
push bx
push cx
push dx
; Main
mov bx, [bp+4] ; Input
cmp bx, 0
jge positive
neg bx ; Negate input, that way we don't have to process negative numbers inside the loop
positive:
xor cx, cx ; loop counter
mov dx, 1
cmp bx, 0
je returnBits

bitsLoop:
inc cx
shl dx, 1 ; Increase power by two
cmp bx, dx ; If our input value is less than a power of two, we can return (dx should be 2^cx)
jl returnBits

cmp cx, 16
jl bitsLoop

returnBits:
; Epilogue
cmp cx, 16
jge SixteenBits
inc cx
sixteenBits:
mov ax, cx ; Move cx+1 into ax
pop dx
pop cx
pop bx
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
push bx
push cx
push dx
; Main
mov bx, [bp+4] ; move possible prime to bx
mov dx, bx
shr dx, 1 ; Move x/2 into dx, max loop count
mov cx, 2 ; Set cx to 2, smallest possible factor for a non-prime
cmp bx, 1 ; if the given number is 1 or less its not a prime
jle isNotPrime
primeCalc:
	cmp cx, dx ; Compare loop counter to x/2
  jg isAPrime ; (This immediately jumps for 2 and 3)
	mov ax, bx ; move the input number into ax for division
  push dx ; Division overwrites DX
  xor dx, dx
	div cx    ; divide the input number by cx (Looping variable)
  or dx, dx ; Compare dx (input % loopCounter) to 0
  pop dx ; Restore dx from division
	jz isNotPrime
	inc cx ; Move cx up one, this counts from 2 to x/2
  jmp primecalc ; Unconditional jump, there's a condition at the beginning.
isAPrime:
	mov ax,1
	jmp endPrime
isNotPrime:
	xor ax, ax ; Set ax to 0
endPrime:
; Epilogue
pop dx
pop cx 
pop bx
mov sp, bp
pop bp
ret
isPrime endp


proc isPerfectSquare
;; Takes value on stack as input, returns a boolean (1/0) of whether or not that number is a perfect square
;; You can simply loop through every number up to sqrt(x), square it (push i, call square, pop i into a different register)
;; If it squares into x, x is a perfect square.
; Prologue
push bp
mov bp, sp
push bx
push cx
; Main
mov bx, [bp+4] ; Moving possible square to bx
mov cx, 181 ; Max square root of a perfect square
cmp bx, 0
jl notSquare ; Perfect squares are always positive
jz isSquare ; 0 is a perfect square by definition
; Fall into squareCalc if it's not any defined case
squareCalc:
	push cx ; cx is our loop variable, it counts down from 181 to 0
	call square ; ax = cx^2
  pop cx ; Pop cx off stack (Simply subtracting stack pointer)
	cmp ax, bx ; Compare ax (cx^2) to bx (Input)
	je isSquare ; If cx^2 == bx, bx is a perfect square
	loop squareCalc ; Loop cx, 181 to 1
  ; If we never find a square, we fall into this
notSquare:
	xor ax, ax ; Clearing ax to 0 to represent boolean false
	jmp squareEnd
isSquare:
	mov ax, 1 ; Move boolean true into ax to return
  ; We fall into function epilogue
squareEnd:
; Epilogue
pop cx
pop bx
mov sp, bp
pop bp
ret
isPerfectSquare endp


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
;; This is equal to the inverse of the least-significant bit
;; Does not use any registers it doesn't return.
  ; Prologue
  push bp
  mov bp, sp
  ; Main
  mov ax, [bp+4] ; Move argument into ax
  and ax, 1b ; Set all bits except for least-significant to 0
  xor ax, 1b ; Invert least-significant bit
  ; Epilogue
  mov sp, bp
  pop bp
  ret
isEven endp


proc printBinary
; Prologue
push bp
mov bp, sp
push ax
push bx
push cx
; Main
mov ax, [bp+4]
push ax
call necessarybits
mov bx, ax
mov cx, 16
sub cx, bx
pop ax ; AX holds the input number, bx holds the number of bits required to display it, cx holds the number of unnecessary bits
shl ax, cl ; Shift left the number of unnecessary bits
mov cx, bx ; Move bit count into cx for looping
binaryPrinter:
  or ax, ax ; Gets highest bit into SF
  jge binaryPrintZero; Jumps if highest bit is 0
  prtDig 1
  jmp binaryPrintShifter
binaryPrintZero:
  prtDig 0
binaryPrintShifter:
  shl ax, 1
  loop binaryPrinter ; Loops for NecessaryBits(x) number of times
; Epilogue
pop cx
pop bx
pop ax
mov sp, bp
pop bp
ret
printBinary endp


proc printOctal
; Prologue
push bp
mov bp, sp
push ax
push bx
push cx
; Main
mov ax, [bp+4]
xor bx, bx ; Flag for leading zeros
mov cx, 5 ; Max number of octal digits for 15-bit int (after sign bit gets extracted)
or ax, ax ; Loads sign bit into SF
jz octalZero ; We want to skip the loop if we have all zeros
jge octalPositive ; Jumps if positive (Don't print negative symbol, still disregard sign bit)
prtChar '-'
neg ax ; Make ax positive
octalPositive:
shl ax, 1 ; Removing sign bit from the left
octalPrinter:
  push cx
  push ax
  and ax, 0E000h ; Bitmask for first 3 bits
  or ax, ax ; Check if ax (first 3 bits) is 0
  jz octalleadingzeros
  octalContinue:
  mov bx, 1 ; Flag that we've scanned a chunk that isn't 0
  mov cl, 13
  shr ax, cl ; Moves bits into least-significant bits
  prtDig al
  octalSkip:
  pop ax
  mov cl, 3
  shl ax, cl
  pop cx
  loop octalprinter
octalEpilogue:
; Epilogue
pop cx
pop bx
pop ax
mov sp, bp
pop bp
ret
octalZero:
  prtChar '0'
  jmp hexepilogue
octalLeadingZeros:
or bx, bx
jz octalSkip ; If we haven't scanned a non-zero number, skip
jmp octalContinue ; Else, print the zero
printOctal endp


proc printHex
;; Goes from -7FFF to 7FFF
; Prologue
push bp
mov bp, sp
push ax
push bx
push cx
; Main
mov ax, [bp+4] ; Input number
xor bx, bx ; Clear bx for leading zeros flag
mov cx, 4 ; Loop number
or ax, ax ; Loads sign bit into SF
jz hexZero ; We want to skip the loop if we have all zeros
jge hexPrinter ; Jumps if positive (Don't print negative symbol, still disregard sign bit)
prtChar '-'
neg ax ; Make ax positive
hexPrinter:
  push cx
  push ax
  and ax, 0F000h ; Bitmask for first 4 bits
  or ax, ax
  jz hexLeadingZeros
  hexContinue:
  mov bx, 1
  mov cl, 12
  shr ax, cl ; Moves bits into least-significant bits
  push ax ; Pushing ax for process call
  call prtHexDig
  pop ax ; Clearing process call from stack (add sp, 2)
  hexSkip:
  pop ax ; Restoring ax from before bitmask
  mov cl, 4 ; Shifting 4 bits left
  shl ax, cl
  pop cx ; Restoring cx for loop counter
  loop hexprinter
hexEpilogue:
; Epilogue
pop cx
pop bx
pop ax
mov sp, bp
pop bp
ret
hexZero:
  prtChar '0'
  jmp hexEpilogue
hexLeadingZeros:
or bx, bx
jz hexskip
jmp hexContinue
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
    xor dx, dx
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