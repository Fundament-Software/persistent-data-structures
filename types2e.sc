# TODO: learn how to read ownership in scopes code
# possibly with the help of lsp type annotations
# TODO: properly learn the usage of let/local
# TODO: safety when making a vector with over 4 billion elements

using import Array
using import enum
using import Rc
# TODO: investigate correct usage of String
using import String
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
    let node-arity = (two-pow block-width)

    fn index-at-depth (index depth)
        let index-shift =
            index >> (block-width * depth)
        mask-bits block-width index-shift

    fn is-tree-full (length depth)
        let root = (depth + 1)
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

type rrb-vector < Struct
    # FIXME: how to return a type of non-mutable data?
    @@ memo
    inline gen-rrb-vector-type (element-type block-width)
        let bit-ops = (gen-bit-ops block-width)

        enum rrb-tree
        let data-node-type =
            Rc (FixedArray element-type bit-ops.node-arity)
        let pointer-node-type =
            Rc (FixedArray rrb-tree bit-ops.node-arity)
        enum rrb-tree
            data-node    : data-node-type
            pointer-node : pointer-node-type

        struct rrb-vector < this-type
            tree   : rrb-tree
            length : u32
            depth  : u32
            # TODO: better way to expose all this?
            let bit-ops rrb-tree data-node-type pointer-node-type

    # TODO: less jank way to do this?
    inline... __typecall
    case (cls element-type)
        this-function cls element-type 5
    case (cls element-type block-width)
        static-assert (cls == this-type) "Use 0 args to construct data"
        gen-rrb-vector-type element-type block-width

    case (cls)
        static-assert (cls != this-type) "Use 1 or 2 args to construct type"
        let tree =
            cls.rrb-tree.data-node (cls.data-node-type)
        let length = 0
        let depth = 0
        Struct.__typecall cls tree length depth

    fn __@ (self index)
        assert (index < self.length) "@ out of bounds!"
        let t = (typeof self)
        # loop instead of recursion
        loop (node depth = (deref self.tree) (deref self.depth))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node data-node
                break (data @ i)
            else
                let-unwrap ptrs node pointer-node
                repeat
                    deref (ptrs @ i)
                    depth - 1

    # TODO: is there a proper method name for this?
    fn update (self index element)
        assert (index < self.length) "update out of bounds!"
        let t = (typeof self)
        fn update-inner (node index depth element) (returning (uniqueof t.rrb-tree -1))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node data-node
                local new-data = (copy data)
                (new-data @ i) = element
                t.rrb-tree.data-node new-data
            else
                let-unwrap ptrs node pointer-node
                local new-ptrs = (copy ptrs)
                let branch =
                    this-function (ptrs @ i) index (depth - 1) element
                (new-ptrs @ i) = branch
                t.rrb-tree.pointer-node new-ptrs
        let tree =
            update-inner self.tree index self.depth element
        let length = self.length
        let depth = self.depth
        Struct.__typecall t tree length depth

    # TODO: mutable when owned
    # TODO: append-front???
    # rn i'm assuming only append-back
    # TODO: extremely similar to update, with extra handling of edge cases
    # refactor?
    fn append (self element)
        let t = (typeof self)

        # make a new branch with a given depth and given first element
        fn new-branch (depth element) (returning (uniqueof t.rrb-tree -1))
            if (depth == 0)
                local data = (t.data-node-type)
                'append data element
                t.rrb-tree.data-node data
            else
                let sub-branch =
                    this-function (depth - 1) element
                local ptrs = (t.pointer-node-type)
                'append ptrs sub-branch
                t.rrb-tree.pointer-node ptrs

        # at a data node, simply copy and append
        # at a pointer node, check whether the branch being touched exists
        # - if not, create it with new-branch
        # - if yes, descend into it, make a copy and replace the branch
        fn append-inner (node index depth element) (returning (uniqueof t.rrb-tree -1))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node data-node
                local new-data = (copy data)
                'append new-data element
                t.rrb-tree.data-node new-data
            else
                let-unwrap ptrs node pointer-node
                local new-ptrs = (copy ptrs)
                if (t.bit-ops.needs-new-branch index depth)
                    let branch =
                        new-branch (depth - 1) element
                    'append new-ptrs branch
                else
                    let branch =
                        this-function (ptrs @ i) index (depth - 1) element
                    (new-ptrs @ i) = branch
                t.rrb-tree.pointer-node new-ptrs

        # if tree is full, make a new root,
        # put tree under it and then the new element
        if (t.bit-ops.is-tree-full self.length self.depth)
            let branch = (new-branch self.depth element)
            local ptrs = (t.pointer-node-type)
            'append ptrs (copy self.tree)
            'append ptrs branch
            let tree = (t.rrb-tree.pointer-node ptrs)
            let length = (self.length + 1)
            let depth = (self.depth + 1)
            Struct.__typecall t tree length depth
        else
            let tree = (append-inner self.tree self.length self.depth element)
            let length = (self.length + 1)
            let depth = self.depth
            Struct.__typecall t tree length depth

    # FIXME: megajank
    fn __repr (self)
        local s = S""
        s ..=
            ..
                "[count="
                repr self.length
                " items="
        s ..=
            # TODO: iterator i?
            loop (i es = 0:u32 S"")
                if (i < self.length)
                    repeat (i + 1)
                        .. es " "
                            repr (self @ i)
                else
                    break es
        s ..=
            " ]"
        s

    # TODO: this is debug stuff, and pretty jank
    # improve or remove?
    # inb4 literally stays forever
    fn compare-reftree (self other)
        fn inner-compare-reftree (prefix depth nodel noder) (returning void)
            if (depth == 0)
                let-unwrap datal nodel data-node
                let-unwrap datar noder data-node
                let lenl = (countof datal)
                let lenr = (countof datar)
                print (.. prefix (repr datal))
                print (.. prefix (repr datar))
            else
                let-unwrap ptrsl nodel pointer-node
                let-unwrap ptrsr noder pointer-node
                let lenl = (countof ptrsl)
                let lenr = (countof ptrsr)
                print (.. prefix (repr ptrsl))
                print (.. prefix (repr ptrsr))
                let minlen = (min lenl lenr)
                for i in (range minlen)
                    this-function (.. prefix (tostring i) " ") (depth - 1) (ptrsl @ i) (ptrsr @ i)
        if (self.depth != other.depth)
            print "FIXME: implement different depths"
        else
            inner-compare-reftree "" self.depth self.tree other.tree

run
    # TODO: turn into a bunch of assertions?
    # test type memoization
    let rrb-vector-i32 = (rrb-vector i32 2)
    let rrb-vector-i32-2 = (rrb-vector i32 2)
    let rrb-vector-i32-3 = (rrb-vector i32 4)
    print (rrb-vector-i32 == rrb-vector-i32-2)
    print (rrb-vector-i32 == rrb-vector-i32-3)
    print (rrb-vector-i32-2 == rrb-vector-i32-3)

    # TODO: refactor
    let my-thing-0 = (rrb-vector-i32)
    let my-thing-1 = ('append my-thing-0 1)
    let my-thing-2 = ('append my-thing-1 2)
    let my-thing-3 = ('append my-thing-2 3)
    let my-thing-4 = ('append my-thing-3 4)
    let my-thing-5 = ('append my-thing-4 5)
    let my-thing-6 = ('append my-thing-5 6)
    'compare-reftree my-thing-5 my-thing-6
    let my-thing-7 = ('append my-thing-6 7)
    let my-thing-8 = ('append my-thing-7 8)
    let my-thing-9 = ('append my-thing-8 9)
    let my-thing-10 = ('append my-thing-9 10)
    let my-thing-11 = ('append my-thing-10 11)
    let my-thing-12 = ('append my-thing-11 12)
    let my-thing-xd = ('update my-thing-12 0:u32 42)
    'compare-reftree my-thing-5 my-thing-6
    print (repr my-thing-0)
    print (repr my-thing-1)
    print (repr my-thing-2)
    print (repr my-thing-3)
    print (repr my-thing-4)
    print (repr my-thing-5)
    print (repr my-thing-6)
    print (repr my-thing-7)
    print (repr my-thing-8)
    print (repr my-thing-9)
    print (repr my-thing-10)
    print (repr my-thing-11)
    print (repr my-thing-12)
    print (repr my-thing-xd)
