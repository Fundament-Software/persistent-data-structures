# an "array" type that is filled with None to capacity
# TODO: element assignment should take element-type rather than Option
# TODO: there are probably better ways to do this
# i just want to get done with this part of the yak

using import Array
using import Option
# TODO: i don't need struct at all
using import struct

typedef SparseArray < Struct

@@ memo
inline gen-type (element-type capacity)
    let Data =
        FixedArray (Option element-type) capacity
    let type-name =
        .. "<SparseArray element-type=" (tostring element-type) "; capacity=" (tostring capacity) ">"

    struct "TODO: this" < SparseArray
        data : Data
        let Data

typedef+ SparseArray
    # TYPECALL
    # TODO: assert is probably not right for usage messages
    inline... __typecall
    case (cls, element-type, capacity)
        static-assert (cls == this-type) "Use no args to construct data"
        gen-type element-type capacity

    case (cls)
        static-assert (cls != this-type) "Use element-type, capacity args to construct type"
        local new-array = (cls.Data)
        let none-data = (cls.Data.ElementType)
        for _ in (range cls.Data.Capacity)
            let none-data-copy = (copy none-data)
            'append new-array none-data-copy
        Struct.__typecall cls
            data = new-array

    # COUNTOF
    inline __countof (self)
        countof self.data

    # AT
    inline __@ (self index)
        self.data @ index

    # SET & CLEAR
    # (convenience)
    inline set (self index element)
        (self @ index) = (Option.wrap element)
    inline clear (self index)
        let t = (typeof self)
        let none-data = (t.Data.ElementType)
        (self @ index) = none-data

    # SPLIT
    # makes two new SparseArrays
    # one is like the original, but everything at index or after is None
    # the other is likewise, but only before index
    inline split (self index)
        let t = (typeof self)
        let index = (index as usize)
        let count = (countof self)
        assert (index <= count) "index out of bounds"
        local new-left = (t)
        local new-right = (t)
        # branches every loop
        # but branch prediction should make it go brrrr
        for i in (range count)
            let element = (self @ i)
            let e-copy = (copy element)
            if (i < index)
                (new-left @ i) = e-copy
            else
                (new-right @ i) = e-copy
        _ new-left new-right

do
    let SparseArray
    locals;
