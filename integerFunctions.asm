;; [bp+4] should hold passed int, after function call and base pointer are pushed onto stack

proc square
; TODO
square endp

proc necessaryBits
; TODO
necessaryBits endp

proc printBinary
; TODO
printBinary endp

proc printOctal
; TODO
printOctal endp

proc printHex
; TODO
printHex endp

printDecimal proc
  ; Prologue
  push bp
  mov bp, sp
  push ax
  ; Main
  mov ax, [bp+4]
  call outdec
  ; Epilogue
  pop ax
  mov sp, bp
  pop bp
  ret
printDecimal endp