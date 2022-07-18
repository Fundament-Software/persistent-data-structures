using import enum

enum Digit
    One : (tuple i32)
    Two : (tuple i32 i32)
    Three : (tuple i32 i32 i32)
    #Four : (tuple i32 i32 i32 i32)

do
    let o = (Digit.One (tupleof 3))
    (dump o)
