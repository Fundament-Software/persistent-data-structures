using import Array
using import .Hamt
using import .run

run
    let hamt-u32 = (Hamt u32)
    let my-hamt = (hamt-u32)
    let array-t = (Array hamt-u32)
    local my-array = (array-t)
    'append my-array my-hamt
    for i in (range 100:u32)
        let k = (i % 32)
        let old-hamt = (my-array @ k)
        let new-hamt = ('set old-hamt k i)
        'append my-array new-hamt
    let my-hamt-first = (my-array @ 0)
    let my-hamt-last = (my-array @ 100)
    let my-thing-xd = ('set my-hamt-last 0 42)
    print (repr my-hamt-first)
    print (repr my-hamt-last)
    print (repr my-thing-xd)
    'print-reftree my-hamt-last
    'print-reftree my-thing-xd

inline id (x)
    x

run
    # test type memoization
    let hamt-u32 = (Hamt u32)
    let hamt-u32-2 = (Hamt u32 u32)
    let hamt-u32-3 = (Hamt u32 u32 id)
    let hamt-u32-4 = (Hamt i32 i32 id i32)
    assert (hamt-u32 == hamt-u32-2)
    assert (hamt-u32 != hamt-u32-3)
    assert (hamt-u32 != hamt-u32-4)
    assert (hamt-u32-2 != hamt-u32-3)
    assert (hamt-u32-2 != hamt-u32-4)
    assert (hamt-u32-3 != hamt-u32-4)
