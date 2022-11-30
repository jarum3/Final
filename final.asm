.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
  ; Arrays
  arr dw 5 dup(?)
  arrLen dw 5
  tempArr dw 5 dup(?) ; Change all of these values at once, they should be equal. (These are 5 just to make testing easier)
    ; Strings for array
    arrDisplayer db 0Ah, 0Dh, "Full array: ", '$'
    arrReversedDisplay db 0Ah, 0Dh, "Reversed array: " , '$'
    arrSortedDisplay db 0Ah, 0Dh, "Sorted array: ", '$'
    arrSumDisplay db 0Ah, 0Dh, "Running total of array: ", '$'
      ; Strings for ints
      currNumDecimal db 0Ah, 0Dh, "-------------------------", 0Ah, 0Dh, "Number: ", '$' ; Print in decimal after this
      currNumBinary db 0Ah, 0Dh, "Number in binary: ", '$'
      currNumOctal db 0Ah, 0Dh, "Number in octal: ", '$'
      currNumHex db 0Ah, 0Dh, "Number in hexadecimal: ", '$'
      currNumBits db 0Ah, 0Dh, "Minimum number of bits to store: ", '$'
      currNumSquare db 0Ah, 0Dh, "Number squared: ", '$'
      currNumNegative db 0Ah, 0Dh, "Number is negative: ", '$'
      currNumPerfectSquare db 0Ah, 0Dh, "Number is a perfect square: ", '$'
      currNumPrime db 0Ah, 0Dh, "Number is prime: ", '$'
      currNumEven db 0Ah, 0Dh, "Number is even: ", '$'
    ; Other strings
    squareErrorMessage db "This number is too large to square.", '$'
.code ; Code segment

include arrFunc.asm
; IOFunc -> intFunc -> arrFunc -> final

main proc ; Main process
  ; Initialize data
	mov ax, @DATA ; Move the segment address for the data segment to ax
	mov ds, ax ; Move the segment address for data from ax to ds (You can only move into dx from a register
  ; Main function
  beginning:
  ; Input grabber
  push offset arr
  push arrLen
  call getArr ; Arr is filled with values input by user
  xor bx, bx ; This will hold current offset for array
  arrProcessing:
  prtStr arrDisplayer
  call prtArr
  ; TODO
  ;prtStr arrSumDisplay
  ;call sumArr
  ;push ax
  ;call printDec
  ;pop ax

  ;push offset arr
  ;push offset tempArr
  ;push arrLen
  ;call reverseArray
  ;prtStr arrReverseDisplay
  ;call prtArr

  ;call selectionSort
  ;prtStr arrSortedDisplay
  ;call prtArr

  intProcessing:
    pause
    mov dx, [arr+bx]
    push dx ; Push current integer onto stack
    ; Display current number with line
    ; Printing
    prtStr currNumDecimal
    call printDec
    prtStr currNumBinary
    call printBinary
    prtStr currNumOctal
    call printOctal
    prtStr currNumHex
    call printHex
    
    ; PRINT(necessaryBits(i));
    ;prtStr currNumBits
    ;call necessaryBits
    ;push ax
    ;call printDec
    ;pop ax
    
    prtStr currNumSquare
    call square
    cmp ax, -1
    je squareOverflow
    push ax
    call printDec
    pop ax
    jmp continueInts
    squareOverflow:
      prtStr squareErrorMessage
    continueInts:
    prtStr currNumNegative
    call isNegative
    push ax
    call prtBool
    pop ax

    prtStr currNumPerfectSquare
    call isPerfectSquare
    push ax
    call prtBool
    pop ax
    
    prtStr currNumPrime
    call isPrime
    push ax
    call prtBool
    pop ax
    
    prtStr currNumEven
    call isEven
    push ax
    call prtBool
    pop ax
    ; Loop ending
    add bx, 2 ; Go to next element
    shr bx, 1
    cmp bx, arrLen
    jge mainExiter
    shl bx, 1
    jmp intProcessing
  mainExiter:
  call exit ; Exits the program
main endp

end main