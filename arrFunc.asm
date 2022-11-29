;; [bp+4] should hold passed length, after function call and base pointer are pushed onto stack
;; [bp+6] should be address to base of array

.data
  elemFetch db 0Ah, 0Dh, "Please enter a value: ", '$'
.code
include intFunc.asm
; IOFunc -> intFunc -> arrFunc -> final
;; Modifies no registers, modifies passed array by filling with user input ints
getArr proc
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
    call getDec ; Grabs number as 16-bit signed int
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
getArr endp

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
    push [bx] ; Moves current pointed-to value into ax to print
    call printDec ; Print value to screen
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

sumArr proc
; TODO
sumArr endp

reverseArray proc
; TODO
reverseArray endp

swapNumsArr proc
; TODO
swapNumsArr endp

selectionSort proc
; TODO
selectionSort endp