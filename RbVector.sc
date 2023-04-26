# TODO: learn how to read memory ownership in scopes code
# possibly with the help of lsp type annotations
# TODO: properly learn the usage of let/local
# also of inline/fn
# TODO: make the error messages clearer
# TODO: investigate possibility of replacing some recursive
# functions with tail-recursion (therefore, loops) by
# moving the work of tail code into the next iteration
# TODO: mutable when owned

# types needed for gen-type
using import Array
using import Option
using import Rc
using import enum
using import struct

# TODO: investigate correct usage of String
using import String

using import .unwrap

inline pow2 (x)
    1 << x

inline mask-bits (n x)
    let mask =
        (pow2 n) - 1
    x & mask

inline gen-bit-ops (radix-size)
    let node-arity = (pow2 radix-size)

    inline index-at-depth (index depth)
        let index-shift =
            index >> (radix-size * depth)
        mask-bits radix-size index-shift

    inline is-tree-full (count depth)
        let root = (depth + 1)
        # node-arity ** root
        let capacity =
            pow2 (radix-size * root)
        count == capacity

    inline needs-new-branch (count depth)
        # is multiple of (node-arity ** depth)
        let remainder =
            mask-bits (radix-size * depth) count
        remainder == 0

    locals;

# TODO: does Rc not support copying its contents?
inline copy-rc-contents (x)
    let rc-type = (typeof x)
    let x-unwrapped = (x as rc-type.Type)
    let x-copy = (copy x-unwrapped)
    Rc.wrap x-copy

# TODO: does Array not support slicing?
inline copy-array-slice (a start end)
    let new-a = ((typeof a))
    for i in (range start end)
        'append new-a (a @ i)
    new-a

typedef RbVector < Struct

# TODO: how to return a type of non-mutable data?
# apparently scopes might simply not allow this
@@ memo
inline gen-type (element-type count-type radix-size)
    let depth-type = u8
    let off-type = u8
    #
        depth only needs to have a max value one less than
        the ceil of count-types's bits divided by
        radix-size, so unless you plan on using something
        larger than u256 and a radix size of 1, or a
        similarly interesting combination, then u8 is fine.
        off-type only needs to have a max value one less
        than (2 ** radix-size). a radix size of >8 is not
        as extreme as the edge case for depth-type, but
        handling it sensibly requires more metaprogramming
        than i know.
        assertions just to alert when my mistakes are
        catching up to me
    static-assert
        ((((sizeof count-type) * 8) - 1) // radix-size) <= depth-type.MAX
        "depth-type is too short (wtf)"
    static-assert
        ((2 ** radix-size) - 1) <= off-type.MAX
        "off-type is too short"
    let bit-ops = (gen-bit-ops radix-size)
    let count-max = count-type.MAX

    enum RbTree

    struct DataNodeType
        data : (Rc (FixedArray element-type bit-ops.node-arity))
        off  : off-type

    struct PointerNodeType
        ptrs   : (Rc (FixedArray RbTree bit-ops.node-arity))
        counts : (Option (Rc (FixedArray count-type bit-ops.node-arity)))
        off    : off-type

    enum RbTree
        DataNode    : DataNodeType
        PointerNode : PointerNodeType

    struct
        .. "<RbVector " (tostring element-type) ">"
        \ < RbVector

        root  : RbTree
        start : count-type
        end   : count-type
        depth : depth-type

        let RbTree DataNodeType PointerNodeType bit-ops count-max

inline new-tree (t root start count depth)
    Struct.__typecall t root start count depth

typedef+ RbVector
    # TYPECALL
    # TODO: assert is not the place to put these messages
    inline... __typecall
    case (cls, element-type, count-type = u32, radix-size : usize = 5)
        static-assert (cls == this-type) "Use 0 args to construct data"
        gen-type element-type count-type radix-size

    case (cls)
        # TODO: init-assignment
        static-assert (cls != this-type) "Use 1 or 3 args to construct type"
        let root =
            cls.RbTree.DataNode (cls.DataNodeType)
        new-tree cls root 0 0 0

    # COUNTOF
    inline __countof (self)
        self.end - self.start

    # AT
    inline... __@ (self, index : usize)
        let t = (typeof self)
        let count = (countof self)
        assert (index < count) "index out of bounds!"
        let index = (index + self.start)

        # loop instead of recursion
        loop (node depth = (deref self.root) (deref self.depth))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let i-data = (i - data.off)
                break (data.data @ i-data)
            else
                let-unwrap ptrs node PointerNode
                let i-ptrs = (i - ptrs.off)
                repeat
                    deref (ptrs.ptrs @ i-ptrs)
                    depth - 1

    # UPDATE
    # TODO: is there a proper method name for this?
    # or like, is `set` a better name?
    inline... update (self, index : usize, element)
        let t = (typeof self)
        let count = (countof self)
        assert (index < count) "index out of bounds!"
        let index = (index + self.start)

        fn update-inner (node index depth element)
            returning (uniqueof t.RbTree -1)

            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let i-data = (i - data.off)
                let new-data = (copy data)
                let new-data-node = (copy-rc-contents data.data)
                (new-data-node @ i-data) = element
                new-data.data = new-data-node
                t.RbTree.DataNode new-data
            else
                let-unwrap ptrs node PointerNode
                let i-ptrs = (i - ptrs.off)
                let sub-branch =
                    this-function (ptrs.ptrs @ i-ptrs) index (depth - 1) element
                let new-ptrs = (copy ptrs)
                let new-ptrs-node = (copy-rc-contents ptrs.ptrs)
                (new-ptrs-node @ i-ptrs) = sub-branch
                new-ptrs.ptrs = new-ptrs-node
                t.RbTree.PointerNode new-ptrs

        let root = (update-inner self.root index self.depth element)
        new-tree t root self.start self.end self.depth

    # APPEND
    # TODO: append-front???
    # rn i'm assuming only append-back
    # i have conflicting ideas for how to implement append-front
    # particularly in the case where we need to re-root the tree
    # do i accept negative indexes?
    # or do i shift all the existing indexes way over to the right?
    # if i shift, how much do i shift?
    # the paper's implementation seems to suggest a shift that
    # puts the old root in the second child of the root
    # which i can't imagine being very intuitive (why not the (radix)th?)
    inline append (self element)
        let t = (typeof self)
        assert (self.end < t.count-max) "count-type is about to overflow!"

        # make a new branch with a given depth and given first element
        fn new-branch (depth element)
            returning (uniqueof t.RbTree -1)

            if (depth == 0)
                let data = (t.DataNodeType)
                'append data.data element
                t.RbTree.DataNode data
            else
                let sub-branch =
                    this-function (depth - 1) element
                let ptrs = (t.PointerNodeType)
                'append ptrs.ptrs sub-branch
                t.RbTree.PointerNode ptrs

        # at a data node, simply copy and append
        # at a pointer node, check whether the branch being touched exists
        # - if not, create it with new-branch
        # - if yes, descend into it, make a copy and replace the branch
        fn append-inner (node index depth element)
            returning (uniqueof t.RbTree -1)

            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let i-data = (i - data.off)
                let new-data = (copy data)
                let new-data-node = (copy-rc-contents data.data)
                'append new-data-node element
                new-data.data = new-data-node
                t.RbTree.DataNode new-data
            else
                let-unwrap ptrs node PointerNode
                let i-ptrs = (i - ptrs.off)
                let new-ptrs = (copy ptrs)
                let new-ptrs-node = (copy-rc-contents ptrs.ptrs)
                if (t.bit-ops.needs-new-branch index depth)
                    let sub-branch =
                        new-branch (depth - 1) element
                    'append new-ptrs-node sub-branch
                else
                    let sub-branch =
                        this-function (ptrs.ptrs @ i-ptrs) index (depth - 1) element
                    (new-ptrs-node @ i-ptrs) = sub-branch
                new-ptrs.ptrs = new-ptrs-node
                t.RbTree.PointerNode new-ptrs

        # if tree is full, make a new root,
        # put tree under it and then the new element
        if (t.bit-ops.is-tree-full self.end self.depth)
            let next-branch = (new-branch self.depth element)
            let ptrs = (t.PointerNodeType)
            'append ptrs.ptrs (copy self.root)
            'appent ptrs.ptrs next-branch
            let root = (t.RbTree.PointerNode ptrs)
            new-tree t root self.start (self.end + 1) (self.depth + 1)
        else
            let root = (append-inner self.root self.end self.depth element)
            new-tree t root self.start (self.end + 1) self.depth

    # SPLIT
    # the rrbvector paper chooses to write this function in
    # a way that it returns only either the left or right
    # sides of the split. i choose to return both always
    #inline split (self index)
        let t = (typeof self)
        let count = (countof self)
        assert (index < count) "index out of bounds!"
        let index = (index + self.start)

        fn split-inner (node index depth)
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let data-count = (countof data)
                let left right = ('split data i)
                _ (t.RbTree.DataNode left) (t.RbTree.DataNode right)
            else
                # the paper apparently tries to optimize splits that
                # lie near a branch boundary, but it does so in a
                # way that causes the tree to become unbalanced so
                # i'm not implementing that optimization
                # TODO: it might've been an attempt to optimize
                # splits near the start/end of the vector
                let-unwrap ptrs node PointerNode
                let ptrs-count = (countof ptrs)
                let left right = ('split ptrs i)
                let subleft subright =
                    this-function (ptrs @ i) index (depth - 1)
                'append left subleft
                (right @ 0) = subright

    # TAKE & DROP
    #inline take (self index)
        let left _ =
            split self index
        left
    #inline drop (self index)
        let _ right =
            split self index
        right

    # REPR
    # FIXME: megajank
    inline __repr (self)
        let count = (countof self)
        local s = S""
        s ..=
            ..
                "[count="
                repr count
                " items="
        s ..=
            # TODO: iterator i?
            loop (i es = 0:u32 S"")
                if (i < count)
                    repeat (i + 1)
                        .. es " "
                            repr (self @ i)
                else
                    break es
        s ..=
            " ]"
        s

    # PRINT-REFTREE
    # debugging, might remove later
    inline print-reftree (self)
        fn inner-reftree (prefix node depth) (returning void)
            if (depth == 0)
                let-unwrap data node DataNode
                print (prefix .. (repr data))
            else
                let-unwrap ptrs node PointerNode
                print (prefix .. (repr ptrs))
                let count = (countof ptrs)
                for i in (range count)
                    this-function (.. prefix (tostring i) " ") (ptrs @ i) (depth - 1)
        inner-reftree "" self.root self.depth

do
    let RbVector
    locals;
