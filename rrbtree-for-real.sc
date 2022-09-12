using import enum
using import struct

type VectorInner < Enum
    inline gen-vector-inner-type (element-type)
        enum
            .. "<VectorInner " (tostring element-type) ">"
            \ < this-type

            Inline :

type Vector < Struct
    inline gen-vector-type (element-type)
        struct
            .. "<Vector " (tostring element-type) ">"
            \ < this-type

            vector : (VectorInner element-type)

    inline __typecall (cls element-type)
        static-if (cls == this-type)
            gen-vector-type element-type
        else
            Struct.__typecall cls
