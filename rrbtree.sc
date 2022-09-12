# TODO understand how `using` works
# ideally i'd import into an identifier rather than global scope
using import Array
using import enum
using import Rc
using import struct

let block-width = 5
let node-arity = (2 ** block-width)
let block-mask = (node-arity - 1)

type RrbTree < Struct
    inline gen-rrb-tree-type (element-type)
        enum BranchOrLeaf
            Branch : (Rc this-type)
            Leaf   : element-type

        struct
            .. "<RrbTree " (tostring element-type) ">"
            \ < this-type

            node : (FixedArray BranchOrLeaf node-arity)

    inline __typecall (cls element-type)
        static-if (cls == this-type)
            gen-rrb-tree-type element-type
        else
            Struct.__typecall cls

    fn get-radix (self idx level)
        let index = ((idx >> (level * block-width)) & block-mask)
        if (level == 0)
            # TODO unwrap Leaf or panic
            self.node @ index
        else
            # TODO unwrap Branch or panic
            'get-radix (self.node @ index) idx (level - 1)

type RrbVector < Struct
    inline gen-rrb-vector-type (element-type)
        struct
            .. "<RrbVector " (tostring element-type) ">"
            \ < this-type

            tree : (RrbTree element-type)
            depth : usize

    inline __typecall (cls element-type)
        static-if (cls == this-type)
            gen-rrb-vector-type element-type
        else
            Struct.__typecall cls

    fn hello-world (self greeting)
        .. greeting ", world!"
            " depth "
            tostring self.depth
            " root count "
            tostring
                countof self.tree.node

do
    let rrbvector = (RrbVector i32)
    dump rrbvector
    let mydata = (rrbvector)
    report
        'hello-world mydata "hello"
