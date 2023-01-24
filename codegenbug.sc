using import Array
using import struct

type struct-thing
    inline __typecall (element-type)
        let inner-type =
            FixedArray element-type 1
        struct "thing"
            m : inner-type
            let inner-type

let struct-thing-i32 =
    struct-thing i32

let x =
    struct-thing-i32.inner-type;

;
