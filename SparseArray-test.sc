using import Option
using import .SparseArray
using import .run

inline noop ()

inline printarray (a)
    let count = (countof a)
    print (typeof a)
    for i in (range count)
        dispatch (a @ i)
        case Some (x)
            print
                ..
                    tostring i
                    ": "
                    tostring x
        default
            noop;

run
    let spa-t = (SparseArray i8 32)
    local spa = (spa-t)
    let count = (countof spa)
    (spa @ 16) = (Option.wrap 42:i8)
    (spa @ 7) = (Option.wrap -128:i8)
    printarray spa
    for i in (range count)
        (spa @ i) = (Option.wrap (i as i8))
    printarray spa
    let left right = ('split spa 12)
    printarray left
    printarray right
