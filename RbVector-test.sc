using import Array
using import .RbVector
using import .run

run
    let rb-vector-i32 = (RbVector i32)
    let my-rb-vector = (rb-vector-i32)
    let array-t = (Array rb-vector-i32)
    local my-array = (array-t)
    'append my-array my-rb-vector
    for i in (range 100)
        let old-rb-vector = (my-array @ i)
        let new-rb-vector = ('append old-rb-vector i)
        'append my-array new-rb-vector
    let my-rb-vector-first = (my-array @ 0)
    let my-rb-vector-last = (my-array @ 100)
    let my-thing-xd = ('update my-rb-vector-last 0:u32 42)
    print (repr my-rb-vector-first)
    print (repr my-rb-vector-last)
    print (repr my-thing-xd)
    'print-reftree my-rb-vector-last
    'print-reftree my-thing-xd
    #print (my-thing-11 == my-thing-12)

run
    # test type memoization
    let rb-vector-i32 = (RbVector i32 u32 2)
    let rb-vector-i32-2 = (RbVector i32 u32 2)
    let rb-vector-i32-3 = (RbVector i32 u64 2)
    let rb-vector-i32-4 = (RbVector i32 u32 4)
    assert (rb-vector-i32 == rb-vector-i32-2)
    assert (rb-vector-i32 != rb-vector-i32-3)
    assert (rb-vector-i32 != rb-vector-i32-4)
    assert (rb-vector-i32-2 != rb-vector-i32-3)
    assert (rb-vector-i32-2 != rb-vector-i32-4)
    assert (rb-vector-i32-3 != rb-vector-i32-4)

#run
    # TODO: assert that this crashes (correctly)
    let tiny-type = u8
    let rb-vector-tiny = (RbVector i32 tiny-type 2)
    local my-thing-big = (rb-vector-tiny)
    for i in (range (tiny-type.MAX + 1))
        my-thing-big = ('append my-thing-big i)
    print (repr my-thing-big)
