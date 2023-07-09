using import Array

let Hamt = (import .Hamt)
using import .run

run
    let hamt-i32 = (Hamt i32)
    let my-hamt = (hamt-i32)
    let array-t = (Array hamt-i32)
    local my-array = (array-t)
    'append my-array my-hamt
    for i in (range 100)
        let k = (i as u32)
        let old-hamt = (my-array @ i)
        let new-hamt = ('set old-hamt k i)
        'append my-array new-hamt
    let my-hamt-first = (my-array @ 0)
    let my-hamt-one = (my-array @ 1)
    let my-hamt-thirty-two = (my-array @ 32)
    let my-hamt-thirty-three = (my-array @ 33)
    let my-hamt-sixty-four = (my-array @ 64)
    let my-hamt-sixty-five = (my-array @ 65)
    let my-hamt-last = (my-array @ 100)
    let my-thing-xd = ('set my-hamt-last 0:u32 42)
    let my-thing-xd2 = ('set my-hamt-thirty-three 1024:u32 69)
    print 0
    print (repr my-hamt-first)
    'print-reftree my-hamt-first
    print 1
    print (repr my-hamt-one)
    'print-reftree my-hamt-one
    print 32
    print (repr my-hamt-thirty-two)
    'print-reftree my-hamt-thirty-two
    print 33
    print (repr my-hamt-thirty-three)
    'print-reftree my-hamt-thirty-three
    print 64
    print (repr my-hamt-sixty-four)
    'print-reftree my-hamt-sixty-four
    print 65
    print (repr my-hamt-sixty-five)
    'print-reftree my-hamt-sixty-five
    print 100
    print (repr my-hamt-last)
    'print-reftree my-hamt-last
    print 100
    print (repr my-thing-xd)
    'print-reftree my-thing-xd
    print 1024
    print (repr my-thing-xd2)
    'print-reftree my-thing-xd2
    #print (my-hamt-thirty-three @ 0:u32)
    #print (my-thing-xd2 @ 0:u32)

inline id-hash (a x)
    a + x

run
    # test type memoization
    let hamt-i32 = (Hamt i32)
    let hamt-i32-2 = (Hamt i32 u32)
    let hamt-i32-3 = (Hamt i32 u32 id-hash)
    let hamt-u32-4 = (Hamt u32 i32 id-hash i32)
    assert (hamt-i32 == hamt-i32-2)
    assert (hamt-i32 != hamt-i32-3)
    assert (hamt-i32 != hamt-u32-4)
    assert (hamt-i32-2 != hamt-i32-3)
    assert (hamt-i32-2 != hamt-u32-4)
    assert (hamt-i32-3 != hamt-u32-4)
