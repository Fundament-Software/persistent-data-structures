using import Array
using import struct

inline gen-thing-type (element-type)
    let inner-type =
        struct "inner-thing"
            x : element-type
    struct "thing"
        m : inner-type
        let inner-type

let struct-thing-i32 =
    gen-thing-type i32

let x =
    struct-thing-i32.inner-type;

;
