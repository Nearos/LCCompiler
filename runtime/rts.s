.include "runtime/macros.s"


.text

.global shallow_string_copy
shallow_string_copy:
    Function
    PushD x1
    mov x1, 16 // 16 bytes for a string struct 
    bl malloc 
    PopD x2
    ldr x3, [x2]
    str x3, [x1]
    ldr x3, [x2, 8]
    str x3, [x1, 8]
    Return 
.global c_malloc 
c_malloc:
    Function
    mov x1, x0 
    bl malloc 
    mov x0, x1
    Return

.global malloc
malloc: // size_t x1 -> void*
    Function
    //mmap(nullptr, size, 
    //      PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS,
    //      -1, 0)
    mov x8, 222 //mmap
    mov x0, 0 //x0 = nullptr
    //x1 = size already
    mov x2, PROT_RW //PROT_READ|PROT_WRITE
    mov x3, MAP_PRIVATE_ANONYMOUS//MAP_PRIVATE|MAP_ANONYMOUS
    mov x4, -1
    mov x5, 0
    svc 0
    mov x1, x0 //return x0
    Return 

.global index_str
index_str: // string a1, size_t a2 -> char* a1
    Function 
    ldr x3, [x1] // size_t a3 = a1.length
    cmp x2, x3
    bge index_str__return0
    cmp x2, 0
    blt index_str__return0
    ldr x1, [x1, 8] // char* a1 = a1.data 
    add x1, x1, x2 // a1 += a2
    Return
index_str__return0:
    add x1, xzr, xzr
    Return

.global print_message
print_message: // string* x1
    Function
    mov x7, x1
    mov x8, 64
    mov x0,  1
    ldp x2, x1, [x7] //field with offset 0 is string length
    svc 0 
    Return

.global itoa
itoa:
    ldr x1, [x1]


.global exit
exit:
    Function
    mov x8, 93
    mov x0, 0
    svc 0
    Return //should not be reached

.global print_newline 
print_newline:
    Function 
    ldr x1, =newline_string_struc
    bl print_message
    Return

.data 

.global append_string_string_struct
append_string_string_struct:
    .quad 0
    .quad 0

newline_string_struc:
    .quad 1
    .quad newline_string_content

newline_string_content:
    .byte 10