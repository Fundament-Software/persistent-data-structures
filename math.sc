# scopes's infix binary operations resolve what type to operate on, by trying---
# in turn---to implicitly coerce the right operand's type to the left's, then
# the left's to the right's. this makes sense on commutative operations, but
# makes less sense on operations like shifts, where the operands each have a
# distinct function and don't need to be the same type, and i expect the return
# to have the same type as the left operand regardless of the right.

# // can be either sdiv or udiv (signed and unsigned integer division)
# depending on whether the operands are signed or unsigned
inline div-fix (l r)
    let t = (typeof l)
    # usize is missing MIN/MAX properties
    static-if (t == usize)
        assert (r <= -1:usize)
        assert (r >= 0)
    #elseif (t == isize)
        apparently doesn't exist
    else
        assert (r <= t.MAX)
        assert (r >= t.MIN)
    let r = (r as t)
    l // r

# shl and shr have nearly the same characteristics
inline shift-fix (op l r)
    let t = (typeof l)
    let b =
        (sizeof t) * 8
    assert (r <= b) # don't shift by too many bits
    assert (r >= 0) # shifts are broken with a negative rhs
    let r = (r as t)
    op l r

# << is safe-shl
# TODO: what safety does safe-shl provide?
# TODO: how to pass infix operator as argument
inline shl-fix (l r)
    let shl = <<
    shift-fix shl l r

# >> can be either ashr or lshr (arithmetic or logical shift-right)
# depending on whether the operands are signed or unsigned
inline shr-fix (l r)
    let shr = >>
    shift-fix shr l r

# other useful operations

# bits stuff

inline pow2 (t x)
    shl-fix (1 as t) x

inline bit-at (a i)
    let shift = (shr-fix a i)
    shift & 1

inline bit-set (a i)
    let t = (typeof a)
    let bit = (pow2 t i)
    a | bit

# TODO: optimize
inline ctpop (a i)
    local n = 0:usize
    for x in (range i)
        n += (bit-at a x)
    n

inline mask-bits (x n)
    let t = (typeof x)
    let mask =
        (pow2 t n) - 1
    x & mask

# arithmetic stuff

inline ceil-div (l r)
    (div-fix (l - 1) r) + 1

locals;
