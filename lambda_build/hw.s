
.text

.text
lambda_body_3: 
    str x29, [sp, -8]!
    str x30, [sp, -8]!

    //load argument into register
    mov x3, x1
    ldr x2, =register_spill_0
    str x3, [x2]

    //load closure environment into registers
    ldr x3, [x0, 0]
    ldr x2, =register_spill_1
    str x3, [x2]

    //function body expr

    //move result into return register
    ldr x2, =register_spill_1
    ldr x3, [x2]
    mov x1, x3
    ldr x30, [sp], 8
    ldr x29, [sp], 8
ret
lambda_body_2: 
    str x29, [sp, -8]!
    str x30, [sp, -8]!

    //load argument into register
    mov x3, x1
    ldr x2, =register_spill_2
    str x3, [x2]

    //load closure environment into registers

    //function body expr

    //Initializing closure

    //Allocating closure struct
    mov x1, 16
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_3
    str x3, [x2]

    //loading function body address
    ldr x3, =lambda_body_3
    ldr x2, =register_spill_4
    str x3, [x2]

    //allocating closure context struct
    mov x1, 8
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_5
    str x3, [x2]

    //populating closure context struct from current context
    ldr x2, =register_spill_2
    ldr x3, [x2]
    ldr x2, =register_spill_5
    ldr x4, [x2]
    str x3, [x4, 0]

    //populating closure struct
    ldr x2, =register_spill_4
    ldr x3, [x2]
    ldr x2, =register_spill_3
    ldr x4, [x2]
    str x3, [x4, 0]
    ldr x2, =register_spill_5
    ldr x3, [x2]
    ldr x2, =register_spill_3
    ldr x4, [x2]
    str x3, [x4, 8]

    //move result into return register
    ldr x2, =register_spill_3
    ldr x3, [x2]
    mov x1, x3
    ldr x30, [sp], 8
    ldr x29, [sp], 8
ret
lambda_body_1: 
    str x29, [sp, -8]!
    str x30, [sp, -8]!

    //load argument into register
    mov x3, x1
    ldr x2, =register_spill_6
    str x3, [x2]

    //load closure environment into registers
    ldr x3, [x0, 0]
    ldr x2, =register_spill_7
    str x3, [x2]

    //function body expr

    //Load closure arg
    ldr x2, =register_spill_6
    ldr x3, [x2]
    mov x1, x3

    //load closure context
    ldr x2, =register_spill_7
    ldr x3, [x2]
    ldr x0, [x3, 8]

    //load function address
    ldr x2, =register_spill_7
    ldr x3, [x2]
    ldr x4, [x3, 0]
    ldr x2, =register_spill_8
    str x4, [x2]

    //branch to fn
    ldr x2, =register_spill_8
    ldr x3, [x2]
    blr x3

    //move result
    mov x3, x1
    ldr x2, =register_spill_9
    str x3, [x2]

    //move result into return register
    ldr x2, =register_spill_9
    ldr x3, [x2]
    mov x1, x3
    ldr x30, [sp], 8
    ldr x29, [sp], 8
ret
lambda_body_0: 
    str x29, [sp, -8]!
    str x30, [sp, -8]!

    //load argument into register
    mov x3, x1
    ldr x2, =register_spill_10
    str x3, [x2]

    //load closure environment into registers

    //function body expr

    //Initializing closure

    //Allocating closure struct
    mov x1, 16
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_11
    str x3, [x2]

    //loading function body address
    ldr x3, =lambda_body_1
    ldr x2, =register_spill_12
    str x3, [x2]

    //allocating closure context struct
    mov x1, 8
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_13
    str x3, [x2]

    //populating closure context struct from current context
    ldr x2, =register_spill_10
    ldr x3, [x2]
    ldr x2, =register_spill_13
    ldr x4, [x2]
    str x3, [x4, 0]

    //populating closure struct
    ldr x2, =register_spill_12
    ldr x3, [x2]
    ldr x2, =register_spill_11
    ldr x4, [x2]
    str x3, [x4, 0]
    ldr x2, =register_spill_13
    ldr x3, [x2]
    ldr x2, =register_spill_11
    ldr x4, [x2]
    str x3, [x4, 8]

    //move result into return register
    ldr x2, =register_spill_11
    ldr x3, [x2]
    mov x1, x3
    ldr x30, [sp], 8
    ldr x29, [sp], 8
ret
    .global _start
_start: 

    //Initializing closure

    //Allocating closure struct
    mov x1, 16
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_14
    str x3, [x2]

    //loading function body address
    ldr x3, =lambda_body_0
    ldr x2, =register_spill_15
    str x3, [x2]

    //allocating closure context struct
    mov x1, 0
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_16
    str x3, [x2]

    //populating closure context struct from current context

    //populating closure struct
    ldr x2, =register_spill_15
    ldr x3, [x2]
    ldr x2, =register_spill_14
    ldr x4, [x2]
    str x3, [x4, 0]
    ldr x2, =register_spill_16
    ldr x3, [x2]
    ldr x2, =register_spill_14
    ldr x4, [x2]
    str x3, [x4, 8]

    //Initializing closure

    //Allocating closure struct
    mov x1, 16
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_17
    str x3, [x2]

    //loading function body address
    ldr x3, =lambda_body_2
    ldr x2, =register_spill_18
    str x3, [x2]

    //allocating closure context struct
    mov x1, 0
    bl malloc
    mov x3, x1
    ldr x2, =register_spill_19
    str x3, [x2]

    //populating closure context struct from current context

    //populating closure struct
    ldr x2, =register_spill_18
    ldr x3, [x2]
    ldr x2, =register_spill_17
    ldr x4, [x2]
    str x3, [x4, 0]
    ldr x2, =register_spill_19
    ldr x3, [x2]
    ldr x2, =register_spill_17
    ldr x4, [x2]
    str x3, [x4, 8]

    //load string "result"
    ldr x3, =string_struct_5
    ldr x2, =register_spill_20
    str x3, [x2]

    //Load closure arg
    ldr x2, =register_spill_20
    ldr x3, [x2]
    mov x1, x3

    //load closure context
    ldr x2, =register_spill_17
    ldr x3, [x2]
    ldr x0, [x3, 8]

    //load function address
    ldr x2, =register_spill_17
    ldr x3, [x2]
    ldr x4, [x3, 0]
    ldr x2, =register_spill_21
    str x4, [x2]

    //branch to fn
    ldr x2, =register_spill_21
    ldr x3, [x2]
    blr x3

    //move result
    mov x3, x1
    ldr x2, =register_spill_22
    str x3, [x2]

    //Load closure arg
    ldr x2, =register_spill_22
    ldr x3, [x2]
    mov x1, x3

    //load closure context
    ldr x2, =register_spill_14
    ldr x3, [x2]
    ldr x0, [x3, 8]

    //load function address
    ldr x2, =register_spill_14
    ldr x3, [x2]
    ldr x4, [x3, 0]
    ldr x2, =register_spill_23
    str x4, [x2]

    //branch to fn
    ldr x2, =register_spill_23
    ldr x3, [x2]
    blr x3

    //move result
    mov x3, x1
    ldr x2, =register_spill_24
    str x3, [x2]

    //load string "notIt"
    ldr x3, =string_struct_7
    ldr x2, =register_spill_25
    str x3, [x2]

    //Load closure arg
    ldr x2, =register_spill_25
    ldr x3, [x2]
    mov x1, x3

    //load closure context
    ldr x2, =register_spill_24
    ldr x3, [x2]
    ldr x0, [x3, 8]

    //load function address
    ldr x2, =register_spill_24
    ldr x3, [x2]
    ldr x4, [x3, 0]
    ldr x2, =register_spill_26
    str x4, [x2]

    //branch to fn
    ldr x2, =register_spill_26
    ldr x3, [x2]
    blr x3

    //move result
    mov x3, x1
    ldr x2, =register_spill_27
    str x3, [x2]
    ldr x2, =register_spill_27
    ldr x3, [x2]
    mov x1, x3
    bl print_message
    bl print_newline
    bl exit

.data
string_content_4: 
    .ascii "result"
string_struct_5: 
    .quad 6
    .quad string_content_4
string_content_6: 
    .ascii "notIt"
string_struct_7: 
    .quad 5
    .quad string_content_6

.data
register_spill_0: 
    .quad 0
register_spill_1: 
    .quad 0
register_spill_2: 
    .quad 0
register_spill_3: 
    .quad 0
register_spill_4: 
    .quad 0
register_spill_5: 
    .quad 0
register_spill_6: 
    .quad 0
register_spill_7: 
    .quad 0
register_spill_8: 
    .quad 0
register_spill_9: 
    .quad 0
register_spill_10: 
    .quad 0
register_spill_11: 
    .quad 0
register_spill_12: 
    .quad 0
register_spill_13: 
    .quad 0
register_spill_14: 
    .quad 0
register_spill_15: 
    .quad 0
register_spill_16: 
    .quad 0
register_spill_17: 
    .quad 0
register_spill_18: 
    .quad 0
register_spill_19: 
    .quad 0
register_spill_20: 
    .quad 0
register_spill_21: 
    .quad 0
register_spill_22: 
    .quad 0
register_spill_23: 
    .quad 0
register_spill_24: 
    .quad 0
register_spill_25: 
    .quad 0
register_spill_26: 
    .quad 0
register_spill_27: 
    .quad 0
 