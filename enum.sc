using import enum

enum Digit
    One : i32
    Two : i32 i32

fn thing (a)
    dispatch a
    case One (x)
        print
            tostring x
    case Two (x y)
        print
            ..
                tostring x
                tostring y
    default
        print "AAAA"

do
    let o = (Digit.One 3)
    dump o
    thing o

using import Option

fn thang (b)
    dispatch b
    case Some (x)
        print
            tostring x
    case None
        print "None"
    default
        print "BBBB"

do
    let p = (Option.wrap 3)
    dump p
    thang p
