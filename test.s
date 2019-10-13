count_odd_nodes:                                  # start of function count_odd_nodes
    sub        $sp, $sp, 4
    sw         $a0, 0($sp)
    move       $t0, $sp                           # set arg ptr
    sub        $sp, $sp, 12                       # make space for 3 locals
    move       $t1, $sp                           # set local ptr
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t0)                        # push signed arg 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 0                             # push signed 0
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    sne        $t4, $t5, $t4                      # ne
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    bnez       $t4, con_skip_if0                  # ifgoto con_skip_if0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 0                             # push signed 0
    j          con_return0                        # goto con_return0
con_skip_if0:
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t0)                        # push signed arg 0
    move       $t2, $t4
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed pointer 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t2)                        # push signed struct 0
    move       $a0, $t4
    add        $sp, $sp, 0
    sub        $sp, $sp, 20                       # 5 words for ptrs and $ra
    add        $t4, $sp, 0
    sw         $t0, 16($t4)
    sw         $t1, 12($t4)
    sw         $t2, 8($t4)
    sw         $t3, 4($t4)                        # save ptrs
    sw         $ra, 0($t4)                        # save $ra
    jal        count_odd_nodes                    # call count_odd_nodes
    move       $t4, $v0
    lw         $ra, 0($sp)
    lw         $t3, 4($sp)
    lw         $t2, 8($sp)
    lw         $t1, 12($sp)
    lw         $t0, 16($sp)
    add        $sp, $sp, 20                       # restore $ra and ptrs
    sw         $t4, 0($t1)
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed local 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 4($t2)                        # push signed struct 1
    move       $a0, $t4
    add        $sp, $sp, 0
    sub        $sp, $sp, 20                       # 5 words for ptrs and $ra
    add        $t4, $sp, 0
    sw         $t0, 16($t4)
    sw         $t1, 12($t4)
    sw         $t2, 8($t4)
    sw         $t3, 4($t4)                        # save ptrs
    sw         $ra, 0($t4)                        # save $ra
    jal        count_odd_nodes                    # call count_odd_nodes
    move       $t4, $v0
    lw         $ra, 0($sp)
    lw         $t3, 4($sp)
    lw         $t2, 8($sp)
    lw         $t1, 12($sp)
    lw         $t0, 16($sp)
    add        $sp, $sp, 20                       # restore $ra and ptrs
    sw         $t4, 4($t1)
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed local 1
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t1)                        # push signed local 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 4($t1)                        # push signed local 1
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    add        $t4, $t5, $t4                      # add
    sw         $t4, 8($t1)
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed local 2
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 8($t2)                        # push signed struct 2
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 65535                         # push signed 65535
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    and        $t4, $t5, $t4                      # and
    sw         $t4, 0($t1)
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed local 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t1)                        # push signed local 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 2                             # push signed 2
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    div        $t4, $t5, $t4                      # div
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 0($t1)                        # push signed local 0
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 1                             # push signed 1
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    add        $t4, $t5, $t4                      # add
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 2                             # push signed 2
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    div        $t4, $t5, $t4                      # div
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    seq        $t4, $t5, $t4                      # eq
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    bnez       $t4, con_skip_if1                  # ifgoto con_skip_if1
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 8($t1)                        # push signed local 2
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    li         $t4, 1                             # push signed 1
    lw         $t5, 0($sp)
    add        $sp, $sp, 4
    add        $t4, $t5, $t4                      # add
    sw         $t4, 8($t1)
    lw         $t4, 0($sp)
    add        $sp, $sp, 4                        # pop signed local 2
con_skip_if1:
    sub        $sp, $sp, 4
    sw         $t4, 0($sp)
    lw         $t4, 8($t1)                        # push signed local 2
con_return0:
    move       $v0, $t4
    add        $sp, $t0, 4
    jr         $ra                                # return
