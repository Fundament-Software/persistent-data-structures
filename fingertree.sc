# i don't like using
# TODO: understand exactly what it does, whether it has footguns, and how to not-use it
using import enum

type Digit
    inline __typecall (cls elt)
        enum (.. "Digit " (tostring elt))
            One : elt
            Two : elt elt
            Three : elt elt elt
            #Four : elt elt elt elt

type Node
    inline __typecall (cls elt)
        enum (.. "Node " (tostring elt))
            Node2 : elt elt
            Node3 : elt elt elt

type FingerTree
    inline __typecall (cls elt)
        enum (.. "FingerTree " (tostring elt))
            Empty
            Single : elt
            Deep :
                Digit elt
                this-type (Node elt)
                Digit elt

do
    let di32 = (Digit i32)
    (dump di32)
    let x1 = (di32.One 3)
    (dump x1)
    let ni32 = (Node i32)
    (dump ni32)
    let x2 = (ni32.Node2 3 4)
    (dump x2)
    let fi32 = (FingerTree i32)
    (dump fi32)
    let x3 = (fi32.Empty)
    (dump x3)
