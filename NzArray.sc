using import Array
using import struct

# because i don't know how to type something like "contains a NzArray of any start position"
# the start position will have to be a runtime value
@@ memo
inline gen-type (cls element-type start-type)
    struct
        .. "<NzArray " (tostring element-type) ">"
        \ < cls
        let element-type
        let underlying-array-type = (GrowingArray element-type)
        underlying-array : underlying-array-type
        start-i : start-type

# @@memo isn't smart enough to figure that omitted start-type is the same as specifying i32
inline... gen-type-memo-hack (cls, element-type, start-type = i32)
    gen-type cls element-type start-type

# TODO: create with initial data
inline gen-value (cls start-i)
    Struct.__typecall cls
        start-i = start-i
        underlying-array = (cls.underlying-array-type)

# "Non-zero-indexed array"
typedef NzArray < Struct
    inline __typecall (cls etc...)
        static-if (cls == this-type)
            gen-type-memo-hack cls etc...
        else
            gen-value cls etc...

    inline __countof (self)
        countof self.underlying-array

    inline start-index (self)
        self.start-i

    inline end-index (self)
        self.start-i + (countof self)

    inline __@ (self index)
        assert (index > self.start-i)
        let inner-index = (index - self.start-i)
        let end = (countof self)
        assert (inner-index < end)
        self.underlying-array @ inner-index

    inline append (self value)
        'append self.underlying-array value

    inline insert (self value index)
        assert (index > self.start-i)
        let inner-index = (index - self.start-i)
        let end = (countof self)
        assert (inner-index <= end)
        'insert self.underlying-array value inner-index

    inline split (self index)
        # TODO: with more effort this could be way more efficient, e.g. slicing
        assert (index > self.start-i)
        let inner-index = (index - self.start-i)
        let end = (countof self)
        assert (inner-index <= end)
        let start-left = self.start-i
        let start-right = index
        let cls = (typeof self)
        local nza-left = ((this-type cls.element-type) start-left)
        local nza-right = ((this-type cls.element-type) start-right)
        for i in (range end)
            if (i < inner-index)
                'append nza-left (self.underlying-array @ i)
            else
                'append nza-right (self.underlying-array @ i)
        _ nza-left nza-right
