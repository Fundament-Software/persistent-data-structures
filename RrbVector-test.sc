using import Array
using import .rrbvector
using import .run

run
    let rrb-vector-i32 = (RrbVector i32)
    let my-rrb-vector = (rrb-vector-i32)
    let array-t = (Array rrb-vector-i32)
    local my-array = (array-t)
    'append my-array my-rrb-vector
    for i in (range 100)
        let old-rrb-vector = (my-array @ i)
        let new-rrb-vector = ('append old-rrb-vector i)
        'append my-array new-rrb-vector
    let my-rrb-vector-first = (my-array @ 0)
    let my-rrb-vector-last = (my-array @ 100)
    let my-thing-xd = ('update my-rrb-vector-last 0:u32 42)
    print (repr my-rrb-vector-first)
    print (repr my-rrb-vector-last)
    print (repr my-thing-xd)
    'print-reftree my-rrb-vector-last
    'print-reftree my-thing-xd
    #print (my-thing-11 == my-thing-12)

run
    # test type memoization
    let rrb-vector-i32 = (RrbVector i32 u32 2)
    let rrb-vector-i32-2 = (RrbVector i32 u32 2)
    let rrb-vector-i32-3 = (RrbVector i32 u64 2)
    let rrb-vector-i32-4 = (RrbVector i32 u32 4)
    assert (rrb-vector-i32 == rrb-vector-i32-2)
    assert (rrb-vector-i32 != rrb-vector-i32-3)
    assert (rrb-vector-i32 != rrb-vector-i32-4)
    assert (rrb-vector-i32-2 != rrb-vector-i32-3)
    assert (rrb-vector-i32-2 != rrb-vector-i32-4)
    assert (rrb-vector-i32-3 != rrb-vector-i32-4)

#run
    # TODO: assert that this crashes (correctly)
    let tiny-type = u8
    let rrb-vector-tiny = (RrbVector i32 tiny-type 2)
    local my-thing-big = (rrb-vector-tiny)
    for i in (range (tiny-type.MAX + 1))
        my-thing-big = ('append my-thing-big i)
    print (repr my-thing-big)
