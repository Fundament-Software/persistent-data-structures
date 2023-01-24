using import Array
using import enum
using import struct
using import .run
using import .unwrap

inline... gen-rrb-vector-type
case (element-type)
    this-function element-type 5
case (element-type block-width)
    let node-arity =
        2 ** block-width
    let block-mask =
        node-arity - 1

    enum rrb-tree
    let data-node-type =
        FixedArray element-type node-arity
    let pointer-node-type =
        FixedArray rrb-tree node-arity
    enum rrb-tree
        data-node    : data-node-type
        pointer-node : pointer-node-type

    # TODO: these things should be methods
    # they should also have the proper names
    # this garbage is demo code that needs to be fixed

    struct rrb-vector

    # __typecall
    fn new ()
        rrb-vector
            tree =
                rrb-tree.data-node
                    data-node-type;
            length = 0
            depth = 0

    fn... new-template
    case (self)
        this-function self self.tree
    case (self tree)
        this-function self tree self.length
    case (self tree length)
        this-function self tree length self.depth
    case (self tree length depth)
        rrb-vector tree length depth

    # __@
    fn get (self index)
        fn get-inner (idx level node)
            returning element-type
            let i =
                block-mask &
                    idx >>
                        level * block-width
            if (level == 0)
                unwrap node data-node (data)
                    # TODO: investigate this copy
                    copy
                        data @ i
            else
                this-function idx
                    level - 1
                    unwrap node pointer-node (ptrs)
                        ptrs @ i
        get-inner index self.depth self.tree

    # __repr (i think???)
    fn printify (self)
        local s =
            "[count=" as string
        s ..=
            repr self.length
        s ..= " items="
        for i in (range self.length)
            s ..= " "
            s ..=
                repr
                    get self i
        s ..= " ]"
        s

    # TODO: mutable copy when owned
    fn append (self element)
        # TODO: append interesting
        dispatch self.tree
        case data-node (data)
            local new-data =
                copy data
            'append new-data element
            new-template self
                rrb-tree.data-node new-data
                self.length + 1
        default
            unreachable;

    struct rrb-vector
        tree   : rrb-tree
        length : u32
        depth  : u32
        let new get printify append

run
    let rrb-vector-i32 =
        gen-rrb-vector-type i32
    local my-thing =
        rrb-vector-i32.new;
    for i in (range 7)
        my-thing =
            rrb-vector-i32.append my-thing i
    print
        rrb-vector-i32.printify my-thing
