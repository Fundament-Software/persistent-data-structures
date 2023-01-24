using import Array
using import enum
using import Option
using import Rc
using import struct

using import .run

let block-width = 5
let node-arity = (2 ** block-width)
let block-mask = (node-arity - 1)

type RrbVector < Struct

inline gen-rrb-vector-type (element-type)
    let node-type = (FixedArray (Rc RrbVector) node-arity)

    enum RrbTree
        Leaf : element-type
        Node : node-type

    struct
        ..
            "<RrbVector "
            (tostring element-type)
            ">"
        \ < RrbVector

        let node-type = node-type
        let tree-type = RrbTree

        depth : i32
        tree : RrbTree

type+ RrbVector
    inline __typecall (cls opt)
        static-if (cls == this-type)
            let element-type = opt
            static-assert
                not (none? element-type)
                "Missing element-type. Usage: (RrbVector element-type)"
            gen-rrb-vector-type element-type
        else
            static-assert
                none? opt
                "TODO: initialization"
            Struct.__typecall cls
                depth = 1
                tree =
                    do
                        let empty-node = (cls.node-type)
                        let t = cls.tree-type
                        t.Node empty-node

run
    let rrb-vector-type = (RrbVector i32)
    #let rrb-vector = (rrb-vector-type)
    let node-type = rrb-vector-type.node-type
    let empty-node = (node-type)
