;; [bp+4]: Number of elements in array (length)
;; [bp+6]: Address of int array
;; Returns ax = Largest Value and bx = Index of largest value
;; Picks first value if duplicates exist, negative numbers are less than 0
lrgArr proc
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
  grtElem:
    cmp [bx], ax
    jle notLarger ; Skip saving value if it's not greater
    mov ax, [bx] ; If current elem is greater than ax, move it into ax
    ; cx holds len-index, index = len-cx
    mov dx, [bp+4] ; dx = length
    sub dx, cx ; dx = index = len-cx, dx will always be greater than cx
    notLarger:
      add bx, 2 ; Size of word in bytes
      loop grtElem
  grtReturning:
    mov bx, dx ; move index into bx, ax already holds largest value
  ; Epilogue
  pop dx
  pop cx
  mov sp, bp
  pop bp
  ret
lrgArr endp

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

;; Exits the program, takes no arguments, returns nothing
exit proc 
  mov ax, 4c00h ; Set int 21h to terminate program with exit code 00 (All good)
  int 21h  ; System interrupt to terminate program
  ret ; Return (This is just because it feels nicer)
exit endp