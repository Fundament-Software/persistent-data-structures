using import Array
using import enum
using import Rc
using import struct
using import .run
using import .unwrap

inline... gen-rrb-vector-type
case (element-type)
    this-function element-type 5
case (element-type block-width)
    #let node-arity =
        1 << block-width
    let node-arity =
        2 ** block-width
    #fn node-arity-pow (n)
        1 <<
            block-width * n
    fn node-arity-pow (n)
        node-arity ** n
    let block-mask =
        node-arity - 1
    fn index-at-level (index level)
        block-mask &
            index >>
                block-width *
                    level - 1

    enum rrb-tree
    let data-node-type =
        FixedArray element-type node-arity
    let pointer-node-type =
        FixedArray rrb-tree node-arity
    let data-node-type-rc =
        Rc data-node-type
    let pointer-node-type-rc =
        Rc pointer-node-type
    enum rrb-tree
        data-node    : data-node-type-rc
        pointer-node : pointer-node-type-rc

    # TODO: these things should be methods
    # they should also have the proper names
    # this garbage is demo code that needs to be fixed

    struct rrb-vector

    # __typecall
    fn new ()
        rrb-vector
            tree =
                rrb-tree.data-node
                    data-node-type-rc;
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
    #fn needs-new-branch (length depth)
        0 ==
            length &
                ;
                    node-arity-pow
                        depth - 1
                    \ - 1
    fn needs-new-branch (length depth)
        (length % (node-arity-pow (depth - 1))) == 0

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
                    data-node-type-rc;
                'append data elt
                rrb-tree.data-node
                    data
            else
                let new-child =
                    this-function elt
                        level - 1
                local ptrs =
                    pointer-node-type-rc;
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
                local new-ptrs =
                    copy ptrs
                if (needs-new-branch idx level)
                    'append new-ptrs
                        new-branch elt
                            level - 1
                else
                    (new-ptrs @ i) =
                        this-function elt
                            ptrs @ i
                            idx
                            level - 1
                rrb-tree.pointer-node new-ptrs
        if (is-tree-full self.length self.depth)
            let new-e2 =
                new-branch element self.depth
            local ptrs =
                pointer-node-type-rc;
            'append ptrs
                copy self.tree
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
    let my-thing-8 =
        rrb-vector-i32.append my-thing-7 8
    let my-thing-9 =
        rrb-vector-i32.append my-thing-8 9
    let my-thing-10 =
        rrb-vector-i32.append my-thing-9 10
    let my-thing-11 =
        rrb-vector-i32.append my-thing-10 11
    let my-thing-12 =
        rrb-vector-i32.append my-thing-11 12
    print
        rrb-vector-i32.printify my-thing-0
    print
        rrb-vector-i32.printify my-thing-1
    print
        rrb-vector-i32.printify my-thing-2
    print
        rrb-vector-i32.printify my-thing-3
    print
        rrb-vector-i32.printify my-thing-4
    print
        rrb-vector-i32.printify my-thing-5
    print
        rrb-vector-i32.printify my-thing-6
    print
        rrb-vector-i32.printify my-thing-7
    print
        rrb-vector-i32.printify my-thing-8
    print
        rrb-vector-i32.printify my-thing-9
    print
        rrb-vector-i32.printify my-thing-10
    print
        rrb-vector-i32.printify my-thing-11
    print
        rrb-vector-i32.printify my-thing-12
