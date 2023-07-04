# TODO:
# - learn how to read memory ownership in scopes code
# - mutable when owned
# - properly learn the usage of let/local and inline/fn
# - make the error messages clearer
# - tail calls
# - prepend and split create unbalanced nodes along the left side
#   possible to avoid?

# types needed for gen-type
using import Array
using import Option
using import Rc
using import enum
using import struct

# TODO: investigate correct usage of String
using import String

let math = (import .math)
using import .unwrap

# TODO: slices? views?
fn... array-split (a : FixedArray, i : usize)
    let cls = (typeof a)
    let count = (countof a)
    assert (i <= count) "array-split out of bounds!"

    local l = (cls)
    for n in (range i)
        'append l (a @ n)

    local r = (cls)
    let j = (count - i)
    for n in (range j)
        let m = (n + i)
        'append r (a @ m)

    _ l r

inline... rc-unwrap (rc : Rc)
    let cls = (typeof rc)
    rc as cls.Type

let decorate-inline... = decorate-inline

@@ memo
inline... gen-ops (index-type : type, radix-width : usize)
    let radix-width
    let node-arity = (math.pow2 index-type radix-width)
    let sizes-type = (FixedArray index-type node-arity)

    inline... mask-bits (x : index-type, n : usize)
        let mask =
            (math.pow2 index-type n) - 1
        x & mask

    inline... get-indexes-balanced (index : index-type, depth : usize)
        let with-node =
            radix-width * (depth + 1)
        let without-node = (radix-width * depth)
        let node-index = (mask-bits index with-node)
        let child-index = (math.shr-fix node-index without-node)
        let subtree-complement = (math.shl-fix child-index without-node)
        let subtree-index = (node-index - subtree-complement)
        _ child-index subtree-index

    inline... get-child-index-unbalanced (index : index-type, sizes : sizes-type)
        # binary search is suggested, but i think it's unnecessary?
        # we're dealing with up to like 32 elements here
        # TODO: study this^
        # TODO: i'd like to write a pattern like
        #   (0..n).find(|i| index < sizes[i]).unwrap_or(n - 1)
        # but idk how do in scopes, so a for loop will have to do
        let n = (countof sizes)
        for i in (range n)
            if (index < (sizes @ i))
                return i
        n - 1

    inline... get-subtree-index-unbalanced (index : index-type, sizes : sizes-type, i : usize)
        if (i == 0)
            deref index
        else
            let prev-size =
                sizes @ (i - 1)
            index - prev-size

    inline... get-indexes-unbalanced (index : index-type, sizes : sizes-type)
        let i = (get-child-index-unbalanced index sizes)
        let subindex = (get-subtree-index-unbalanced index sizes i)
        _ i subindex

    locals;

# FIX/ME: how to return a type of non-mutable data?
# conclusion: apparently scopes might simply not allow this
@@ memo
inline... gen-type (cls : type, value-type : type, index-type : type, radix-width : usize)
    let depth-storage-type = u8
    #   depth only needs to have a max value one less than
        the ceil of index-types's bits divided by
        radix-width, so unless you plan on using something
        larger than u256 and a radix width of 1, or a
        similarly interesting combination, then u8 is fine.
        assertion just to alert when i'm wrong
    # FIXME: unbalanced nodes might upset this invariant. investigate!
    let index-bits =
        (sizeof index-type) * 8
    static-assert
        depth-storage-type.MAX >= ((math.ceil-div index-bits radix-width) - 1)
        "depth-storage-type is too short (wtf)"

    struct
        .. "<RrbVector " (tostring value-type) ">"
        \ < cls

        let index-type depth-storage-type value-type
        let ops = (gen-ops index-type radix-width)

        enum RrbTree

        let DataNode =
            FixedArray (Rc value-type) ops.node-arity
        let PointerNode =
            FixedArray (Rc RrbTree) ops.node-arity
        let SizesNode =
            Option (Rc ops.sizes-type)
        struct PSNode
            ptrs  : PointerNode
            sizes : SizesNode
            # ???
            # TODO: derive copy
            fn... __copy (self : this-type)
                this-type
                    ptrs = (copy self.ptrs)
                    sizes = (copy self.sizes)

        enum RrbTree
            data-node : DataNode
            ps-node   : PSNode

        let root-type = (Rc RrbTree)

        let index-max =
            do
                # usize is missing MIN/MAX attributes
                static-if (index-type == usize)
                    -1:usize
                else
                    index-type.MAX

        root  : root-type
        count : index-type
        depth : depth-storage-type

# @@memo isn't smart enough to figure that omitting optional arguments is the
# same as explicitly passing the default values.
# decorators don't support inline... by default though, and defining
# decorate-inline... to decorate-inline probably isn't helping.
# so, in some way, this is expected
inline... gen-type-defaults (cls : type, value-type : type, index-type : type = u32, radix-width : usize = 5)
    gen-type cls value-type index-type radix-width

inline... gen-value-with (cls : type, root, count, depth)
    let root = (imply root cls.root-type)
    let count = (imply count cls.index-type)
    let depth = (imply depth usize)
    assert (depth <= cls.depth-storage-type.MAX) "depth-storage-type overflow!!!"

    let depth = (depth as cls.depth-storage-type)
    Struct.__typecall cls root count depth

# TODO: create with initial data
inline... gen-value (cls : type)
    let root =
        cls.RrbTree.data-node (cls.DataNode)
    gen-value-with cls (Rc.wrap root) 0 0

typedef RrbVector < Struct
    # TYPECALL
    inline... __typecall (cls : type, etc...)
        static-if (cls == this-type)
            gen-type-defaults cls etc...
        else
            gen-value cls etc...

    # GETTERS
    inline... get (self : this-type)
        _
            self.root
            self.count
            (self.depth as usize)

    # COUNTOF, LEN
    inline... __countof (self : this-type)
        self.count

    # AT
    fn... __@ (self : this-type, index)
        let cls = (typeof self)
        let index = (imply index cls.index-type)
        let root count depth = ('get self)
        assert (index < count) "@ out of bounds!"

        # ???
        let value-type = (viewof cls.value-type 1)
        # children of balanced nodes are always balanced
        fn... at-inner-balanced (node : cls.RrbTree, index : cls.index-type, depth : usize)
            returning (_: value-type)
            let i subindex = (cls.ops.get-indexes-balanced index depth)
            if (depth == 0)
                let-unwrap data node data-node
                deref
                    rc-unwrap (data @ i)
            else
                let-unwrap ps node ps-node
                assert (ps.sizes == cls.SizesNode.None) "@ balanced unbalanced!!!"
                this-function (ps.ptrs @ i) subindex (depth - 1)
        #end fn at-inner-balanced
        fn... at-inner-unbalanced (node : cls.RrbTree, index : cls.index-type, depth : usize)
            returning (_: value-type)
            if (depth == 0)
                at-inner-balanced node index depth
            else
                let-unwrap ps node ps-node
                dispatch ps.sizes
                case Some (sizes)
                    assert ((countof ps.ptrs) == (countof sizes)) "@ unbalanced inconsistent!!!"
                    let i subindex = (cls.ops.get-indexes-unbalanced index sizes)
                    this-function (ps.ptrs @ i) subindex (depth - 1)
                case None ()
                    at-inner-balanced node index depth
                default
                    error "@ default!?"
        #end fn at-inner-unbalanced

        at-inner-unbalanced root index depth

    # UPDATE, SET
    # TODO: is there a magic name for ``(self @ index) = value``?
    fn... update (self : this-type, index, value)
        let cls = (typeof self)
        let index = (imply index cls.index-type)
        let value = (imply value cls.value-type)
        let root count depth = ('get self)
        assert (index < count) "update out of bounds!"

        let rrb-tree = (uniqueof cls.RrbTree -1)
        # children of balanced nodes are always balanced
        fn... update-inner-balanced (node : cls.RrbTree, index : cls.index-type, depth : usize, value : cls.value-type)
            returning (_: rrb-tree)
            let i subindex = (cls.ops.get-indexes-balanced index depth)
            if (depth == 0)
                let-unwrap data node data-node
                let new-data = (copy data)
                (new-data @ i) = (Rc.wrap value)
                cls.RrbTree.data-node new-data
            else
                let-unwrap ps node ps-node
                assert (ps.sizes == cls.SizesNode.None) "update balanced unbalanced!!!"
                let new-subtree =
                    this-function (ps.ptrs @ i) subindex (depth - 1) value
                let new-ptrs = (copy ps.ptrs)
                (new-ptrs @ i) = (Rc.wrap new-subtree)
                let new-ps =
                    cls.PSNode (ptrs = new-ptrs)
                cls.RrbTree.ps-node new-ps
        #end fn update-inner-balanced
        fn... update-inner-unbalanced (node : cls.RrbTree, index : cls.index-type, depth : usize, value : cls.value-type)
            returning (_: rrb-tree)
            if (depth == 0)
                update-inner-balanced node index depth value
            else
                let-unwrap ps node ps-node
                dispatch ps.sizes
                case Some (sizes)
                    assert ((countof ps.ptrs) == (countof sizes)) "update unbalanced inconsistent!!!"
                    let i subindex = (cls.ops.get-indexes-unbalanced index sizes)
                    let new-subtree =
                        this-function (ps.ptrs @ i) subindex (depth - 1) value
                    let new-ptrs = (copy ps.ptrs)
                    (new-ptrs @ i) = (Rc.wrap new-subtree)
                    let new-ps =
                        cls.PSNode
                            ptrs = new-ptrs
                            sizes = (copy ps.sizes) # sizes don't change
                    cls.RrbTree.ps-node new-ps
                case None ()
                    update-inner-balanced node index depth value
                default
                    error "update default!?"
        #end fn update-inner-unbalanced

        let new-root = (update-inner-unbalanced root index depth value)
        gen-value-with cls (Rc.wrap new-root) count depth

    # APPEND, PUSHBACK
    # TODO: prepend
    fn... append (self : this-type, value)
        let cls = (typeof self)
        let value = (imply value cls.value-type)
        let root count depth = ('get self)
        assert (count < cls.index-max) "append count overflow!"

        let rrb-tree = (uniqueof cls.RrbTree -1)
        # make a new subtree with a given depth and given first value
        fn... gen-new-subtree (depth : usize, value : cls.value-type)
            returning (_: rrb-tree)
            if (depth == 0)
                local new-data = (cls.DataNode)
                'append new-data (Rc.wrap value)
                cls.RrbTree.data-node new-data
            else
                let new-subtree =
                    this-function (depth - 1) value
                local new-ptrs = (cls.PointerNode)
                'append new-ptrs (Rc.wrap new-subtree)
                # new nodes are always balanced
                let new-ps =
                    cls.PSNode (ptrs = new-ptrs)
                cls.RrbTree.ps-node new-ps
        #end fn gen-new-subtree
        # at a data node, simply copy and append
        # at a pointer node, check whether the subtree being touched exists
        # - if not, create it with gen-new-subtree
        # - if yes, descend into it, make a copy and replace the subtree
        # children of balanced nodes are always balanced
        fn... append-inner-balanced (node : cls.RrbTree, index : cls.index-type, depth : usize, value : cls.value-type)
            returning (_: rrb-tree)
            let i subindex = (cls.ops.get-indexes-balanced index depth)
            if (depth == 0)
                let-unwrap data node data-node
                assert ((countof data) < cls.ops.node-arity) "append data overfull!!!"
                local new-data = (copy data)
                'append new-data (Rc.wrap value)
                cls.RrbTree.data-node new-data
            else
                let-unwrap ps node ps-node
                assert (ps.sizes == cls.SizesNode.None) "append balanced unbalanced!!!"
                local new-ptrs = (copy ps.ptrs)
                if (subindex == 0) # needs new subtree
                    assert ((countof ps.ptrs) == i) "append balanced inconsistent!!!"
                    let new-subtree =
                        gen-new-subtree (depth - 1) value
                    'append new-ptrs (Rc.wrap new-subtree)
                else
                    let new-subtree =
                        this-function (ps.ptrs @ i) subindex (depth - 1) value
                    (new-ptrs @ i) = (Rc.wrap new-subtree)
                let new-ps =
                    cls.PSNode (ptrs = new-ptrs)
                cls.RrbTree.ps-node new-ps
        #end fn append-inner-balanced
        # return either an updated subtree or a new one to append
        # bool notifies which result happened to the caller
        # second bool notifies whether this node is unbalanced; reduces code duplication for re-root
        # TODO: very evident duplication of leaf and balanced-node cases. refactor?
        # TODO: if an unbalanced node is found to be balanced, convert?
        fn... append-inner-unbalanced (node : cls.RrbTree, index : cls.index-type, depth : usize, value : cls.value-type)
            returning (_: rrb-tree bool bool)
            if (depth == 0)
                let _i subindex =
                    cls.ops.get-indexes-balanced index (depth + 1)
                if (subindex == 0) # needs new subtree
                    _ (gen-new-subtree depth value) true false
                else
                    _ (append-inner-balanced node index depth value) false false
            else
                let-unwrap ps node ps-node
                dispatch ps.sizes
                case Some (sizes)
                    assert ((countof ps.ptrs) == (countof sizes)) "append unbalanced inconsistent!!!"
                    let i subindex = (cls.ops.get-indexes-unbalanced index sizes)
                    let new-subtree append _unbalanced =
                        this-function (ps.ptrs @ i) subindex (depth - 1) value
                    if (append and ((countof ps.ptrs) == cls.ops.node-arity))
                        # don't waste work by running gen-new-subtree again
                        local new-ptrs = (cls.PointerNode)
                        'append new-ptrs (Rc.wrap new-subtree)
                        let new-ps =
                            cls.PSNode (ptrs = new-ptrs)
                        # this new node is balanced, but the second true represents the old unbalanced node
                        _ (cls.RrbTree.ps-node new-ps) true true
                    else
                        local new-ptrs = (copy ps.ptrs)
                        let new-sizes = (copy sizes)
                        if append
                            'append new-ptrs (Rc.wrap new-subtree)
                            'append new-sizes
                                Rc.wrap
                                    (sizes @ i) + 1
                        else
                            (new-ptrs @ i) = (Rc.wrap new-subtree)
                            (new-sizes @ i) =
                                Rc.wrap
                                    (sizes @ i) + 1
                        let new-ps =
                            cls.PSNode
                                ptrs = new-ptrs
                                sizes = (Option.wrap new-sizes)
                        _ (cls.RrbTree.ps-node new-ps) false true
                case None ()
                    let _i subindex =
                        cls.ops.get-indexes-balanced index (depth + 1)
                    if (subindex == 0) # needs new subtree
                        _ (gen-new-subtree depth value) true false
                    else
                        _ (append-inner-balanced node index depth value) false false
                default
                    error "append default!?"
        #end fn append-inner-unbalanced

        let new-node append unbalanced = (append-inner-unbalanced root count depth value)
        # if tree is full, make a new root,
        # put tree under it and then the new value
        if (append and (count > 0))
            local new-ptrs = (cls.PointerNode)
            'append new-ptrs (copy root)
            'append new-ptrs (Rc.wrap new-node)
            # new root may be balanced or unbalanced depending on old root
            let new-ps =
                do
                    if unbalanced
                        local new-sizes = (cls.ops.sizes-type)
                        'append new-sizes count
                        'append new-sizes (count + 1)
                        cls.PSNode
                            ptrs = new-ptrs
                            sizes =
                                Option.wrap (Rc.wrap new-sizes)
                    else
                        cls.PSNode (ptrs = new-ptrs)
            let new-root = (cls.RrbTree.ps-node new-ps)
            gen-value-with cls (Rc.wrap new-root) (count + 1) (depth + 1)
        else
            gen-value-with cls (Rc.wrap new-node) (count + 1) depth

    # TODO: WIP
    # SPLIT
    # the rrbvector paper chooses to write this function in a way that it
    # returns only either the left or right sides of the split.
    # i choose to return both always
    fn... split (self : this-type, index)
        let cls = (typeof self)
        let index = (imply index cls.index-type)
        let root count depth = ('get self)
        assert (index <= count) "split out of bounds!"

        let rrb-tree = (uniqueof cls.RrbTree -1)
        # children of balanced nodes are always balanced
        fn... split-inner-balanced (node : cls.RrbTree, index : cls.index-type, depth : usize)
            returning (_: rrb-tree rrb-tree)
            let i subindex = (cls.ops.get-indexes-balanced index depth)
            if (depth == 0)
                let-unwrap data node DataNode
                let left right = (array-split data i)
                _ (cls.RbTree.DataNode left) (cls.RbTree.DataNode right)
            else
                # the paper apparently tries to optimize splits that lie near a
                # branch boundary, but it does so in a way that causes the tree
                # to have an uneven depth, so i'm not implementing that optimization
                # TODO: it might've been an attempt to optimize splits near the
                # start/end of the vector
                let-unwrap ps node ps-node
                let left right = (array-split ps.ptrs i)
                let subleft subright =
                    this-function (ps.ptrs @ i) subindex (depth - 1)
                'append left subleft
                (right @ 0) = subright
                let new-ps-left =
                    cls.PSNode (ptrs = left)
                let new-ps-right =
                    cls.PSNode (ptrs = right)
                _ (cls.RrbTree.ps-node new-ps-left) (cls.RrbTree.ps-node new-ps-right)

    # TAKE, DROP, SKIP
    inline... take (self : this-type, index)
        let left _ = (split self index)
        left
    inline... drop (self : this-type, index)
        let _ right = (split self index)
        right

    # REPR, TOSTRING, PRINT
    fn... __repr (self : this-type)
        let count = (countof self)
        local s = S""
        s ..= "[count="
        s ..= (repr count)
        s ..= " items="
        for i in (range count)
            s ..= " "
            s ..=
                repr (self @ i)
        s ..= " ]"
        s

    # debugging
    inline... print-reftree (self : this-type)
        let cls = (typeof self)
        let root _count depth = ('get self)
        fn... inner-reftree (prefix : String, node : cls.RrbTree, depth : usize)
            returning void
            if (depth == 0)
                let-unwrap data node data-node
                print (prefix .. (repr data))
            else
                let-unwrap ps node ps-node
                dispatch ps.sizes
                case Some (sizes)
                    print (prefix .. (repr ps.ptrs))
                    print (prefix .. (repr sizes))
                    let count = (countof ps.ptrs)
                    for i in (range count)
                        this-function (.. prefix (tostring i) " ") (ps.ptrs @ i) (depth - 1)
                case None ()
                    print (prefix .. (repr ps.ptrs))
                    let count = (countof ps.ptrs)
                    for i in (range count)
                        this-function (.. prefix (tostring i) " ") (ps.ptrs @ i) (depth - 1)
                default
                    error "print-reftree default!?"
        inner-reftree S"" root depth
