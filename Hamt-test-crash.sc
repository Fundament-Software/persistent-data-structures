using import enum
using import Option
using import Rc
using import struct

inline gen-type-bsarray (Value)
    struct (.. "<BsArray " (tostring Value) ">")
        let Value

inline gen-type-hamt ()
    enum Node
    let MB =
        gen-type-bsarray (Rc Node)
    enum Node
        Map-Base : MB
    struct "<Hamt-crash>"
        let MB Node
        root : MB

fn set-hamt (self)
    let cls = (typeof self)
    let o =
        Option (Rc cls.Node)
    fn set-inner (entry)
        returning void
        dispatch entry
        case None () ()
        case Some (node)
            dispatch node
            case Map-Base (mb)
                this-function (o)
            default ()
        default ()
    set-inner (o)

do
    let my-hamt-cls = (gen-type-hamt)
    let my-hamt = (my-hamt-cls)
    set-hamt my-hamt
    ;
