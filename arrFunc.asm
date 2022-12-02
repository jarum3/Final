;; [bp+4] should hold passed length, after function call and base pointer are pushed onto stack
;; [bp+6] should be address to base of array (Or destination array, in those cases)
;; [bp+8] should optionally hold source array, in cases where its applicable

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
  push dx
  ; Main
  mov cx, [bp+4] ; Move array length into cx to loop over it
  mov bx, [bp+6] ; Move base pointer for array into bx
  prtChar '[' ; Start of array
  printElem:
    push [bx] ; Moves current pointed-to value onto stack to print
    call printDec ; Print value to screen
    pop dx ; Collecting value off stack (Just to decrement stack pointer honestly)
    add bx, 2 ; Advance array pointer two bytes (one word)
    cmp cx, 1
    je lastElem ; Only print a comma for formatting if we aren't on the last element (cx=1)
    prtComma
    lastElem: ; Skips past printing a comma on last element
    loop printElem
    prtchar ']' ; End of array
  ; Epilogue
  pop dx
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

sumArr proc
; Prologue
push bp
mov bp, sp
push bx
push cx
; Main
xor ax, ax ; Clear ax to hold sum
mov bx, [bp+6] ; Base address
mov cx, [bp+4] ; Length
sumArrLoop:
  add ax, [bx] ; Add the value held in bx's address to ax (Current elem value)
  add bx, 2 ; Increment pointer
  loop sumarrloop ; Repeat for each element
; Epilogue
pop cx
pop bx
mov sp, bp
pop bp
ret
sumArr endp

reverseArray proc
; Prologue
push bp
mov bp, sp
; Main
; TODO
; Epilogue
mov sp, bp
pop bp
ret
reverseArray endp

copyArr proc
;; Takes two base arrays, and their length, and copies element-by-element to new memory location
; Prologue
push bp
mov bp, sp
push ax
push bx
push cx
push dx
; Main
mov bx, [bp+8] ; Base address of source array
mov cx, [bp+4] ; Length of both arrays
mov dx, [bp+6] ; Base address of destination array
copyArrLoop:
mov ax, [bx]
push bx
mov bx, dx
mov [bx], ax
pop bx
add bx, 2
add dx, 2
loop copyArrLoop
; Epilogue
pop dx
pop cx
pop bx
pop ax
mov sp, bp
pop bp
ret
copyArr endp

swapNumsArr proc
;; Takes two pointers, [bp+4] and [bp+6]
;; Swaps their values
; Prologue
push bp
mov bp, sp
push bx
push cx
push di
push si
; Main
mov di, [bp+4] ; Holds pointer to second num
mov si, [bp+6] ; Holds pointer to first num
mov bx, [di] ; bx grabs value from di pointer (second num)
mov cx, [si] ; cx grabs value from si pointer (first num)
mov [si], bx ; Replaces value at memory address [bp+6] with [bp+4]'s value
mov [di], cx ; Replaces value at memory address [bp+4] with [bp+6]'s value
mov [bp+6], si
mov [bp+4], di
; Epilogue
pop si
pop di
pop cx
pop bx
mov sp, bp
pop bp
ret
swapNumsArr endp

selectionSort proc
; Prologue
push bp
mov bp, sp
push ax
push bx
push cx
; Main
mov bx, [bp+6] ; Holds base of array
mov cx, [bp+4] ; Holds length of array

selectingSort:
;; BX holds address of current elem in array (Also base of array to newly sort)
;; CX holds length of current array (Decrements 1 every loop)
  push bx
  push cx
  call smlArr ; Loads lowest VALUE into ax and its INDEX into bx (Loops over whole array, nested loop)
  pop cx
  mov dx, bx
  pop bx
  shl dx, 1
  add dx, bx
  push bx
  push dx
  call swapNumsArr ; Swaps current (bottom) elem with memory address of lowest value
  pop dx
  pop bx
  add bx, 2
loop selectingSort
; Epilogue
pop cx
pop bx
pop ax
mov sp, bp
pop bp
ret
selectionSort endp