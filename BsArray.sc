using import Array
using import Option
using import String
using import struct

let math = (import .math)

let decorate-inline... = decorate-inline

@@ memo
inline... gen-type (cls : type, Value : type, Bits : type)
    let bits-bits =
        (sizeof Bits) * 8
    # i'm not sure how else to assert that Bits is an unsigned integer type
    # this doesn't work with usize but perhaps don't use usize
    static-assert (Bits.MIN == 0) "Bits is not unsigned"
    # TODO: unbounded compile-time ints? or perhaps a safer way to do this check
    #static-assert (Bits.MAX == ((math.pow2 u64 bits-bits) - 1)) "Bits is not unsigned"
    # maybe this might work
    static-assert (Bits.MAX == (-1 as Bits)) "Bits is not unsigned"
    struct
        .. "<BsArray " (tostring Value) ">"
        \ < cls

        let Value Bits
        let Underlying-Array = (GrowingArray Value)
        let max-length = bits-bits

        underlying-array : Underlying-Array
        bits : Bits

inline... gen-type-defaults (cls : type, Value : type, Bits : type = u32)
    gen-type cls Value Bits

inline... gen-value-with (cls : type, underlying-array, bits)
    let underlying-array = (imply underlying-array cls.Underlying-Array)
    let bits = (imply bits cls.Bits)
    Struct.__typecall cls underlying-array bits

# TODO: create with initial data
inline... gen-value (cls : type)
    gen-value-with cls (cls.Underlying-Array) 0

# "Bitstring-mapped sparse array"
typedef BsArray < Struct
    inline... __typecall (cls : type, etc...)
        static-if (cls == this-type)
            gen-type-defaults cls etc...
        else
            gen-value cls etc...

    fn... __copy (self : this-type)
        let cls = (typeof self)
        gen-value-with cls (copy self.underlying-array) self.bits

    inline... __countof (self : this-type)
        countof self.underlying-array

    fn... __@ (self : this-type, index : usize)
        let cls = (typeof self)
        assert (index < cls.max-length) "@ out of bounds!"

        if ((math.bit-at self.bits index) == 1)
            let count = (math.ctpop self.bits index)
            let value =
                copy (self.underlying-array @ count)
            Option.wrap value
        else
            # TODO: infer none?
            ((Option cls.Value))

    # TODO: is there a magic name for ``(self @ index) = value``?
    fn... set (self : this-type, index : usize, value)
        let cls = (typeof self)
        let value = (imply value cls.Value)
        assert (index < cls.max-length) "set out of bounds!"

        let count = (math.ctpop self.bits index)
        if ((math.bit-at self.bits index) == 1)
            (self.underlying-array @ count) = value
        else
            self.bits = (math.bit-set self.bits index)
            'insert self.underlying-array value count

    fn... __repr (self : this-type)
        local s = S""
        s ..= "["
        s ..= (tostring self.bits)
        s ..= "] { "
        let c = (countof self.underlying-array)
        for i in (range c)
            s ..= (tostring i)
            s ..= ": "
            s ..=
                repr (self.underlying-array @ i)
            s ..= "; "
        s ..= "}"
        s
