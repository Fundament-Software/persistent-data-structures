using import Array
using import Option
using import struct

let decorate-inline... = decorate-inline

@@ memo
inline... gen-ops (bits-type : type)
    inline... bit-at (a : bits-type, i : usize)
        let shift =
            (a >> i) as bits-type
        shift & 1

    # TODO: optimize
    inline... ctpop (a : bits-type, i : usize)
        local n = 0:usize
        for x in (range i)
            n += (bit-at a x)
        n

    inline... bit-set (a : bits-type, i : usize)
        let one = (1 as bits-type)
        let shift =
            (one << i) as bits-type
        shift | a

    locals;

@@ memo
inline... gen-type (cls : type, value-type : type, bits-type : type)
    let bits-bits =
        (sizeof bits-type) * 8
    # i'm not sure how else to assert that bits-type is an unsigned integer type
    # this doesn't work with usize but perhaps don't use usize
    static-assert (bits-type.MIN == 0) "bits-type is not unsigned"
    # TODO: unbounded compile-type ints? or perhaps a safer way to do this check
    #static-assert (bits-type.MAX == ((1:u64 << bits-bits) - 1)) "bits-type is not unsigned"
    # maybe this might work
    static-assert (bits-type.MAX == (-1 as bits-type)) "bits-type is not unsigned"
    struct
        .. "<BsArray " (tostring value-type) ">"
        \ < cls

        let value-type
        let ops = (gen-ops bits-type)
        let underlying-array-type = (GrowingArray value-type)
        let max-length = bits-bits

        underlying-array : underlying-array-type
        bits : bits-type

inline... gen-type-defaults (cls : type, value-type : type, bits-type : type = u32)
    gen-type cls value-type bits-type

# TODO: create with initial data
inline... gen-value (cls : type)
    Struct.__typecall cls
        bits = 0
        underlying-array = (cls.underlying-array-type)

# "Bitstring-mapped sparse array"
typedef BsArray < Struct
    inline... __typecall (cls : type, etc...)
        static-if (cls == this-type)
            gen-type-defaults cls etc...
        else
            gen-value cls etc...

    inline... __countof (self : this-type)
        countof self.underlying-array

    fn... __@ (self : this-type, index : usize)
        let cls = (typeof self)
        assert (index < cls.max-length) "@ out of bounds!"

        if ((cls.ops.bit-at self.bits index) == 1)
            let count = (cls.ops.ctpop self.bits index)
            Option.wrap (self.underlying-array @ count)
        else
            # TODO: infer none?
            ((Option cls.value-type))

    # TODO: is there a magic name for ``(self @ index) = value``?
    fn... set (self : this-type, index : usize, value)
        let cls = (typeof self)
        let value = (imply value cls.value-type)
        assert (index < cls.max-length) "set out of bounds!"

        let count = (cls.ops.ctpop self.bits index)
        if ((cls.ops.bit-at self.bits index) == 1)
            (self.underlying-array @ count) = value
        else
            self.bits = (cls.ops.bit-set self.bits index)
            'insert self.underlying-array value count
