.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
  arr dw 10 dup(?)
  tempArr dw 10 dup(?)
  arrLen dw 10
.code ; Code segment

include integerFunctions.asm
include arrayFunctions.asm
include IOFunctions.asm

main proc ; Main process
  ; Initialize data
	mov ax, @DATA ; Move the segment address for the data segment to ax
	mov ds, ax ; Move the segment address for data from ax to ds (You can only move into dx from a register
  ; Main function
  start:
  call exit ; Exits the program
main endp

;; Exits the program, takes no arguments, returns nothing
exit proc 
  mov ax, 4c00h ; Set int 21h to terminate program with exit code 00 (All good)
  int 21h  ; System interrupt to terminate program
  ret ; Return
exit endp