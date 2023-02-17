using import Array
using import enum
using import Rc
using import struct
using import .run
using import .unwrap

inline two-pow (n)
    1 << n

inline mask-bits (n x)
    let mask =
        (two-pow n) - 1
    x & mask

inline gen-bit-ops (block-width)
    let node-arity =
        two-pow block-width

    fn index-at-depth (index depth)
        let index-shift =
            index >> (block-width * depth)
        mask-bits block-width index-shift

    fn is-tree-full (length depth)
        let root =
            depth + 1
        # node-arity ** root
        let capacity =
            two-pow (block-width * root)
        length == capacity

    fn needs-new-branch (length depth)
        # is multiple of (node-arity ** depth)
        let remainder =
            mask-bits (block-width * depth) length
        remainder == 0

    locals;

inline... gen-rrb-vector-type
case (element-type)
    this-function element-type 5
case (element-type block-width)
    let bit-ops =
        gen-bit-ops block-width

    enum rrb-tree
    let data-node-type =
        Rc (FixedArray element-type bit-ops.node-arity)
    let pointer-node-type =
        Rc (FixedArray rrb-tree bit-ops.node-arity)
    enum rrb-tree
        data-node    : data-node-type
        pointer-node : pointer-node-type

    # TODO: these things should be methods
    # they should also have the proper names
    # this garbage is demo code that needs to be fixed

    struct rrb-vector

    # __typecall
    fn new ()
        let tree =
            rrb-tree.data-node (data-node-type)
        let length = 0
        let depth = 0
        rrb-vector tree length depth

    # __@
    fn get (self index)
        # loop instead of recursion
        loop (node depth = self.tree self.depth)
            let i =
                bit-ops.index-at-depth index depth
            if (depth == 0)
                let-unwrap data node data-node
                break (data @ i)
            else
                let-unwrap ptrs node pointer-node
                repeat
                    deref (ptrs @ i)
                    depth - 1

    # __repr (i think???)
    fn printify (self)
        local s =
            ..
                "[count="
                repr self.length
                " items="
        for i in (range self.length)
            s =
                ..
                    s
                    " "
                    repr (get self i)
        s ..= " ]"
        s

    # TODO: mutable when owned
    # TODO: append-front???
    # rn i'm assuming only append-back
    fn append (self element)
        # make a new branch with a given depth and given first element
        fn new-branch (depth element) (returning (uniqueof rrb-tree -1))
            if (depth == 0)
                local data =
                    data-node-type;
                'append data element
                rrb-tree.data-node data
            else
                let sub-branch =
                    this-function (depth - 1) element
                local ptrs =
                    pointer-node-type;
                'append ptrs sub-branch
                rrb-tree.pointer-node ptrs

        # at a data node, simply copy and append
        # at a pointer node, check whether the branch being touched exists
        # - if not, create it with new-branch
        # - if yes, descend into it, make a copy and replace the branch
        fn append-inner (node index depth element) (returning (uniqueof rrb-tree -1))
            let i =
                bit-ops.index-at-depth index depth
            if (depth == 0)
                let-unwrap data node data-node
                local new-data =
                    copy data
                'append new-data element
                rrb-tree.data-node new-data
            else
                let-unwrap ptrs node pointer-node
                local new-ptrs =
                    copy ptrs
                if (bit-ops.needs-new-branch index depth)
                    let branch =
                        new-branch (depth - 1) element
                    'append new-ptrs branch
                else
                    let branch =
                        this-function (ptrs @ i) index (depth - 1) element
                    (new-ptrs @ i) = branch
                rrb-tree.pointer-node new-ptrs

        if (bit-ops.is-tree-full self.length self.depth)
            let branch =
                new-branch self.depth element
            local ptrs =
                pointer-node-type;
            'append ptrs (copy self.tree)
            'append ptrs branch
            let tree =
                rrb-tree.pointer-node ptrs
            let length =
                self.length + 1
            let depth =
                self.depth + 1
            rrb-vector tree length depth
        else
            let tree =
                append-inner self.tree self.length self.depth element
            let length =
                self.length + 1
            let depth =
                self.depth
            rrb-vector tree length depth

    struct rrb-vector
        tree   : rrb-tree
        length : u32
        depth  : u32
        let new get append printify

run
    let rrb-vector-i32 =
        gen-rrb-vector-type i32 2
    # bunch of lets to showcase persistence
    # TODO: how do i know it's not copying everything all the time?
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
    print (rrb-vector-i32.printify my-thing-0)
    print (rrb-vector-i32.printify my-thing-1)
    print (rrb-vector-i32.printify my-thing-2)
    print (rrb-vector-i32.printify my-thing-3)
    print (rrb-vector-i32.printify my-thing-4)
    print (rrb-vector-i32.printify my-thing-5)
    print (rrb-vector-i32.printify my-thing-6)
    print (rrb-vector-i32.printify my-thing-7)
    print (rrb-vector-i32.printify my-thing-8)
    print (rrb-vector-i32.printify my-thing-9)
    print (rrb-vector-i32.printify my-thing-10)
    print (rrb-vector-i32.printify my-thing-11)
    print (rrb-vector-i32.printify my-thing-12)
