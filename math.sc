# TODO: there must be a better method for this
inline bits-of (t)
    # only correct for unsigned integers
    static-assert (t.MIN == 0) "TODO: bits-of other types"
    loop (b x = 0 t.MAX)
        if (x > 0)
            repeat (b + 1) (x >> 1)
        else
            break b

inline pow2 (x)
    1 << x

do
    let bits-of pow2
    locals;
