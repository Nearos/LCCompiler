.macro Function
    stp x29, x30, [sp, -16]! //put ra on the stack
.endm

.macro Return
    ldp x29, x30, [sp], 16
    ret
.endm

.macro PushD reg 
    add sp, sp, -8
    str \reg, [sp]
.endm 

.macro PopD reg 
    ldr \reg, [sp]
    add sp, sp, 8
.endm


.equ PROT_READ, 1
.equ PROT_WRITE, 2
.equ PROT_RW, 3

.equ MAP_SHARED, 1
.equ MAP_PRIVATE, 2

.equ MAP_ANONYMOUS, 0x20

.equ MAP_PRIVATE_ANONYMOUS, MAP_PRIVATE | MAP_ANONYMOUS
