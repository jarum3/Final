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
  prtStr arrDisplayer ; Printing out the initial array
  call prtArr

  prtStr arrSumDisplay ; Printing out the sum
  call sumArr
  push ax ; AX holds return of sum function, pushing that to printDec
  call printDec
  pop ax ; Grabbing ax again

  push offset arr ; Base address of array holding user inputs
  push offset tempArr ; Base address of empty array
  push arrLen ; Length of both arrays
  call copyArr ; Copies arr -> tempArr
  call reverseArray ; Reverses tempArr
  prtStr arrReversedDisplay ; Prints reversed array
  call prtArr

  ;; Sorts tempArray in-place
  call selectionSort
  prtStr arrSortedDisplay
  call prtArr ; Prints sorted array

  intProcessing:
    pause ; Pause at the beginning of every loop
    mov dx, [arr+bx] ; Move the current integer into dx
    push dx ; Push current integer onto stack
    ; Display current number with line
    ; Printing, self explanatory
    prtStr currNumDecimal
    call printDec
    prtStr currNumBinary
    call printBinary
    prtStr currNumOctal
    call printOctal
    prtStr currNumHex
    call printHex
    
    ; TODO
    prtStr currNumBits
    call necessaryBits
    push ax
    call printDec
    pop ax
    
    prtStr currNumSquare
    call square
    cmp ax, -1 ; numSquare returns -1 for overflow, (Squared ints can't be negative)
    ; Technically this overflow could be a bit larger (181 -> 256) if we used an unsigned int, but all of these work with signed ints as-is
    je squareOverflow ; Display an overflow message
    push ax ; Print if a number was returned
    call printDec
    pop ax
    jmp continueInts ; Skip over the overflow message
    squareOverflow:
      prtStr squareErrorMessage ; Print a message saying that the number was too large
    continueInts:
    ; Back to printing
    prtStr currNumNegative
    call isNegative
    push ax
    call prtBool ; Prints True or False for an integer
    pop ax

    ; TODO
    prtStr currNumPerfectSquare
    call isPerfectSquare
    push ax
    call prtBool
    pop ax
    
    ; TODO
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
    shr bx, 1 ; Convert byte-count to length
    cmp bx, arrLen ; Check if we've gone above length (after incrementing)
    jge mainExiter ; If we've  gone above length, exit the function
    shl bx, 1 ; Return bx to byte-count if we haven't jumped
    jmp intProcessing ; Loop (This should be unconditional so we can actually reach the top of the program again)
  mainExiter:
  call exit ; Exits the program
main endp

end main