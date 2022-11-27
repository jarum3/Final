.model small ; Set memory
.stack	100h ; Set stack size
.data ; Data segment
  arr dw 10 dup(?)
  tempArr dw 10 dup(?)
  arrLen equ $ - arr
.code ; Code segment

include arrFunc.asm
; IOFunc -> intFunc -> arrFunc -> final

main proc ; Main process
  ; Initialize data
	mov ax, @DATA ; Move the segment address for the data segment to ax
	mov ds, ax ; Move the segment address for data from ax to ds (You can only move into dx from a register
  ; Main function
  start:
  push arr
  push arrLen
  call getArr
  call exit ; Exits the program
main endp

end main