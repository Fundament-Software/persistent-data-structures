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
        1 << block-width
    fn node-arity-pow (n)
        1 <<
            block-width * n
    let block-mask =
        node-arity - 1
    fn index-at-level (index level)
        block-mask &
            index >>
                block-width * level

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
            depth = 1

    fn... new-template
    case (self)
        this-function self self.tree
    case (self tree)
        this-function self tree self.length
    case (self tree length)
        this-function self tree length self.depth
    case (self tree length depth)
        rrb-vector tree length depth

    # if we ever do append-front then this
    # might be better named needs-new-root
    fn is-tree-full (length depth)
        length ==
            node-arity-pow depth

    # TODO: i'm writing this with tired and shitpost brain
    # delete and incinerate this asap
    # and then rewrite more sensibly
    fn needs-new-branch (length depth)
        0 ==
            length &
                ;
                    node-arity-pow
                        depth - 1
                    \ - 1

    # __@
    fn get (self index)
        # loop instead of recursion
        loop (level node = self.depth self.tree)
            let i =
                index-at-level index level
            if (level == 1)
                let-unwrap data node data-node data
                break
                    data @ i
            else
                let-unwrap ptrs node pointer-node ptrs
                repeat
                    level - 1
                    deref
                        ptrs @ i

    # TODO: mutable when owned
    # TODO: append-front???
    # rn i'm assuming only append-back
    fn append (self element)
        fn new-branch (elt level) (returning (uniqueof rrb-tree -1))
            if (level == 1)
                local data =
                    data-node-type;
                'append data elt
                rrb-tree.data-node
                    data
            else
                let new-child =
                    this-function elt
                        level - 1
                local ptrs =
                    pointer-node-type;
                'append ptrs new-child
                rrb-tree.pointer-node
                    ptrs
        fn append-inner (elt node idx level) (returning (uniqueof rrb-tree -1))
            let i =
                index-at-level idx level
            if (level == 1)
                let-unwrap data node data-node data
                local new-data =
                    copy data
                'append new-data elt
                rrb-tree.data-node new-data
            else
                let-unwrap ptrs node pointer-node ptrs
                let new-child =
                    if (needs-new-branch idx level)
                        new-branch elt
                            level - 1
                    else
                        this-function elt
                            ptrs @ i
                            idx
                            level - 1
                local new-ptrs =
                    copy ptrs
                (new-ptrs @ i) = new-child
                rrb-tree.pointer-node new-ptrs
        if (is-tree-full self.length self.depth)
            let new-e2 =
                new-branch element self.depth
            local ptrs =
                pointer-node-type;
            'append ptrs self.tree
            'append ptrs new-e2
            rrb-vector
                tree =
                    rrb-tree.pointer-node
                        ptrs
                length =
                    self.length + 1
                depth =
                    self.depth + 1
        else
            new-template self
                append-inner element self.tree self.length self.depth
                self.length + 1

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

    struct rrb-vector
        tree   : rrb-tree
        length : u32
        depth  : u32
        let new get append printify

run
    let rrb-vector-i32 =
        gen-rrb-vector-type i32 2
    # bunch of lets to showcase persistence
    let my-thing-0 =
        rrb-vector-i32.new;
    let my-thing-1 =
        rrb-vector-i32.append my-thing-0 1
    let my-thing-2 =
        rrb-vector-i32.append my-thing-1 2
    let my-thing-3 =
        rrb-vector-i32.append my-thing-2 3
    let my-thing-4 =
        rrb-vector-i32.append my-thing-3 4
    let my-thing-5 =
        rrb-vector-i32.append my-thing-4 5
    let my-thing-6 =
        rrb-vector-i32.append my-thing-5 6
    let my-thing-7 =
        rrb-vector-i32.append my-thing-6 7
    print
        rrb-vector-i32.printify my-thing-7
