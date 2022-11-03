.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
	msg1 db "Please enter a number: ", '$'
  msg2 db "Your number is: ", '$'
  msg3 db "Would you like to run again (y/n): ", '$'
  msg4 db "Bye!", '$'
  lbk2 db 0Ah, 0Dh, 0Ah, 0Dh, '$' ; Initialize $-terminated string holding two line breaks 
    ; (Prints a blank line)
  input db 5, ?, 5 dup (?) ; Initialize input buffer for 6 characters
.code ; Code segment
main proc ; Main process
  ; Initialize data
	mov ax, @DATA ; Move the segment address for the data segment to ax
	mov ds, ax ; Move the segment address for data from ax to ds (You can only move into dx from a register
  ; Main function
  start:
  push offset msg1 ; Print message prompting user to input a string
  call printString
  call getInputNum ; Get user input, saves in ax
  call blankLine ; Print a blink line
  push offset msg2 ; Print message framing hailstones
  call printString
  push ax ; Pushes input hailstone to stack for Collatz Process
  call printString
  push offset msg3
  call printString
  call getInputChar ; Get character for input
  cmp al, 'y' ; If user inputs y, repeat (all other inputs are treated as n)
  call blankLine ; Prints a blank line
  je start ; Returns to start of program
  push offset msg4 ; Prints goodbye message
  call printString
  call blankLine ; Prints a blank line
  call exit ; Exits the program
main endp

;; Takes an argument of a memory address for a $-terminated string on stack [bp+4], prints to screen, returns nothing
printString proc
  ; Prologue (Saving registers to stack so that this function doesn't alter state)
  push bp ; Save bp at entry on stack
  mov bp, sp ; Move stack pointer into bp to maintain it
  push ax ; Move ax at entry on stack
  push dx ; Move dx at entry on stack
  ; Main
  mov dx, [bp+4] ; Move the passed argument memory address into dx
  mov ah, 9 ; set int 21h to print $-terminated string from dx
  int 21h ; System interrupt, prints $-terminated string from memory offset at dx in segment ds
  ; Cleanup (Returning values to registers to maintain state)
  pop dx ; Return state of dx
  pop ax ; Return state of ax
  mov sp, bp ; Return state of sp
  pop bp ; Return state of bp
  ret ; Return
printString endp


;; Takes an argument on the stack [bp+4], prints to screen, returns nothing
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


;; Takes no arguments, gets single character from user, returns that value in ax
getInputChar proc
  ; Main
  mov ah, 01h ; Setting for interrupt
  int 21h ; Moves input char to AL
  xor ah, ah ; Clears ah, ax = al
  ret
getInputChar endp


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


;; Function to print a blank line (Only used for convenience)
blankLine proc
  ; Store stack pointer
  push bp
  mov bp, sp
  ; Print lbk2
  push offset lbk2
  call printString
  ; Restore stack pointer
  mov sp, bp
  pop bp
  ret
blankLine endp


;; Exits the program, takes no arguments, returns nothing
exit proc 
  mov ax, 4c00h ; Set int 21h to terminate program with exit code 00 (All good)
  int 21h  ; System interrupt to terminate program
  ret ; Return
exit endp