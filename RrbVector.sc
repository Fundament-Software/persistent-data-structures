# TODO: learn how to read memory ownership in scopes code
# possibly with the help of lsp type annotations
# TODO: properly learn the usage of let/local
# also of inline/fn
# TODO: make the error messages clearer
# TODO: investigate possibility of replacing some recursive
# functions with tail-recursion (therefore, loops) by
# moving the work of tail code into the next iteration

# types needed for gen-type
using import Array
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

inline gen-bit-ops (block-width)
    let node-arity = (pow2 block-width)

    inline index-at-depth (index depth)
        let index-shift =
            index >> (block-width * depth)
        mask-bits block-width index-shift

    inline is-tree-full (count depth)
        let root = (depth + 1)
        # node-arity ** root
        let capacity =
            pow2 (block-width * root)
        count == capacity

    inline needs-new-branch (count depth)
        # is multiple of (node-arity ** depth)
        let remainder =
            mask-bits (block-width * depth) count
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

typedef RrbVector < Struct

# TODO: how to return a type of non-mutable data?
# apparently scopes might simply not allow this
@@ memo
inline gen-type (element-type count-type block-width)
    let depth-type = u8
    #
        depth only needs to have a max value one less than
        the ceil of count-types's bits divided by
        block-width, so unless you plan on using something
        larger than u256 and a block width of 1, or a
        similarly interesting combination, then u8 is fine.
        assertion just to alert when i'm wrong
    static-assert
        depth-type.MAX >= (((sizeof count-type) - 1) // block-width)
        "depth-type is too short (wtf)"
    let bit-ops = (gen-bit-ops block-width)
    let count-max = count-type.MAX

    enum RrbTree

    let DataNodeType =
        Rc (FixedArray element-type bit-ops.node-arity)
    let PointerNodeType =
        Rc (FixedArray RrbTree bit-ops.node-arity)

    enum RrbTree
        DataNode    : DataNodeType
        PointerNode : PointerNodeType

    struct
        .. "<RrbVector " (tostring element-type) ">"
        \ < RrbVector

        root  : RrbTree
        count : count-type
        depth : depth-type

        let RrbTree DataNodeType PointerNodeType bit-ops count-max

typedef+ RrbVector
    # TYPECALL
    # TODO: assert is not the place to put these messages
    inline... __typecall
    case (cls, element-type, count-type = u32, block-width : usize = 5)
        static-assert (cls == this-type) "Use 0 args to construct data"
        gen-type element-type count-type block-width

    case (cls)
        static-assert (cls != this-type) "Use 1 or 3 args to construct type"
        let root =
            cls.RrbTree.DataNode (cls.DataNodeType)
        let count = 0
        let depth = 0
        Struct.__typecall cls root count depth

    # COUNTOF
    inline __countof (self)
        self.count

    # AT
    inline __@ (self index)
        let t = (typeof self)
        let count = (countof self)
        assert (index < count) "@ out of bounds!"
        # loop instead of recursion
        loop (node depth = (deref self.root) (deref self.depth))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                break (data @ i)
            else
                let-unwrap ptrs node PointerNode
                repeat
                    deref (ptrs @ i)
                    depth - 1

    # UPDATE
    # TODO: is there a proper method name for this?
    # or like, is set a better name?
    inline update (self index element)
        let t = (typeof self)
        let count = (countof self)
        assert (index < count) "update out of bounds!"

        fn update-inner (node index depth element) (returning (uniqueof t.RrbTree -1))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let new-data = (copy-rc-contents data)
                (new-data @ i) = element
                t.RrbTree.DataNode new-data
            else
                let-unwrap ptrs node PointerNode
                let new-ptrs = (copy-rc-contents ptrs)
                let branch =
                    this-function (ptrs @ i) index (depth - 1) element
                (new-ptrs @ i) = branch
                t.RrbTree.PointerNode new-ptrs

        let root = (update-inner self.root index self.depth element)
        #let count = self.count
        let depth = self.depth
        Struct.__typecall t root count depth

    # APPEND
    # TODO: mutable when owned
    # TODO: append-front???
    # rn i'm assuming only append-back
    inline append (self element)
        let t = (typeof self)
        let count = (countof self)
        assert (count < t.count-max) "count-type is about to overflow!"

        # make a new branch with a given depth and given first element
        fn new-branch (depth element) (returning (uniqueof t.RrbTree -1))
            if (depth == 0)
                let data = (t.DataNodeType)
                'append data element
                t.RrbTree.DataNode data
            else
                let sub-branch =
                    this-function (depth - 1) element
                let ptrs = (t.PointerNodeType)
                'append ptrs sub-branch
                t.RrbTree.PointerNode ptrs

        # at a data node, simply copy and append
        # at a pointer node, check whether the branch being touched exists
        # - if not, create it with new-branch
        # - if yes, descend into it, make a copy and replace the branch
        fn append-inner (node index depth element) (returning (uniqueof t.RrbTree -1))
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let new-data = (copy-rc-contents data)
                'append new-data element
                t.RrbTree.DataNode new-data
            else
                let-unwrap ptrs node PointerNode
                let new-ptrs = (copy-rc-contents ptrs)
                if (t.bit-ops.needs-new-branch index depth)
                    let branch =
                        new-branch (depth - 1) element
                    'append new-ptrs branch
                else
                    let branch =
                        this-function (ptrs @ i) index (depth - 1) element
                    (new-ptrs @ i) = branch
                t.RrbTree.PointerNode new-ptrs

        # if tree is full, make a new root,
        # put tree under it and then the new element
        if (t.bit-ops.is-tree-full self.count self.depth)
            let branch = (new-branch self.depth element)
            let ptrs = (t.PointerNodeType)
            'append ptrs (copy self.root)
            'append ptrs branch
            let root = (t.RrbTree.PointerNode ptrs)
            let count = (self.count + 1)
            let depth = (self.depth + 1)
            Struct.__typecall t root count depth
        else
            let root = (append-inner self.root self.count self.depth element)
            let count = (self.count + 1)
            let depth = self.depth
            Struct.__typecall t root count depth

    # SPLIT
    # the rrbvector paper chooses to write this function in
    # a way that it returns only either the left or right
    # sides of the split. i choose to return both always
    inline split (self index)
        let t = (typeof self)
        fn split-inner (node index depth)
            let i = (t.bit-ops.index-at-depth index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let data-count = (countof data)
                let left = (copy-array-slice data 0 i)
                let right = (copy-array-slice data i node-count)
                _ (t.RrbTree.DataNode left) 0 (t.RrbTree.DataNode right) 0
            else
                let-unwrap ptrs node PointerNode
                let ptrs-count = (countof ptrs)
                let left = (copy-array-slice ptrs 0 i)
                let right = (copy-array-slice ptrs i node-count)
                let subleft sldepth subright srdepth =
                    this-function (ptrs @ i) index (depth - 1)

    # TAKE & DROP
    inline take (self index)
        let left _ =
            split self index
        left
    inline drop (self index)
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
    let RrbVector
    locals;
