using import Array
using import .Hamt
using import .run

run
    let hamt-u32 = (Hamt u32)
    let my-hamt = (hamt-u32)

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
