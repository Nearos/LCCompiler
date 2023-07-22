.include "runtime/macros_macos.s"


.text

.align 3
.global _main
_main:
    b _start

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
    mov x16, 197 //mmap
    mov x0, 0 //x0 = nullptr
    //x1 = size already
    add x1, x1, 16
    mov x2, PROT_RW //PROT_READ|PROT_WRITE
    mov x3, MAP_PRIVATE_ANONYMOUS//MAP_PRIVATE|MAP_ANONYMOUS
    mov x4, -1
    mov x5, 0
    svc 0x80
    mov x1, x0 //return x0
    add x1, x1, 8
    mov x2, 7
    mvn x2, x2
    and x1, x1, x2
    cmp x1, 0
    beq exit
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
    mov x16, 4 //macos write syscall
    mov x0,  1
    ldr x2, [x7] //field with offset 0 is string length
    ldr x1, [x7, 8]
    svc 0x80
    Return

.global itoa
itoa:
    ldr x1, [x1]


.global exit
exit:
    Function
    mov x16, 1 //macos syscall
    mov x0, 0
    svc 0x80
    Return //should not be reached

.global print_newline 
print_newline:
    Function 
    adrp x1, newline_string_struc@PAGE
    add x1, x1, newline_string_struc@PAGEOFF
    bl print_message
    Return

panic: 
    Function
    adrp x1, panic_string_struct@PAGE
    add x1, x1, panic_string_struct@PAGEOFF
    bl print_message
    bl exit
    Return 

.data 

.global append_string_string_struct

.align 3
append_string_string_struct:
    .quad 0
    .quad 0

.align 3
newline_string_struc:
    .quad 1
    .quad newline_string_content

.align 3
newline_string_content:
    .byte 10

.align 3
panic_string_struct:
    .quad 6
    .quad panic_string_content

.align 3
panic_string_content: 
    .ascii "panic!"