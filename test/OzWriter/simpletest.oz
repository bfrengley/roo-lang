    call proc_main
    halt
proc_main:
    push_stack_frame 3
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    call_builtin read_int
    load_address r1, 0
    store_indirect r1, r0
    call_builtin read_int
    load_address r1, 1
    store_indirect r1, r0
    load r1, 0
    load r2, 1
    mul_int r0, r1, r2
    call_builtin print_int
    pop_stack_frame 3
    return
