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
.code ; Code segment

include arrFunc.asm
; IOFunc -> intFunc -> arrFunc -> final

main proc ; Main process
  ; Initialize data
	mov ax, @DATA ; Move the segment address for the data segment to ax
	mov ds, ax ; Move the segment address for data from ax to ds (You can only move into dx from a register
  ; Main function
  start:
  ; Input grabber
  push arr
  push arrLen
  call getArr ; Arr is filled with values input by user
  pop cx ; Move array length into cx
  xor bx, bx ; This will hold current offset for array
  intProcessing:
    mov dx, [arr+bx]
    push dx ; Push current integer onto stack
    ; Display current number with line
    prtStr currNumDecimal
    call printDec
    
    ; Loop ending
    add bx, 2 ; Go to next element
    loop intProcessing ; This executes for arrLen times, so it goes over everything in the array.
  call exit ; Exits the program
main endp

end main