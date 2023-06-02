# types needed for gen-type
using import Array
using import Option
using import Rc
using import enum
using import struct

using import String

using import .unwrap

inline copy-contents (x)
    let rc-type = (typeof x)
    let x-unwrapped = (x as rc-type.Type)
    let x-copy = (copy x-unwrapped)
    Rc.wrap x-copy

inline id-hash (a x)
    a + x

inline bit-at (a i)
    (a >> i) & 1

# TODO: optimize
inline ctpop (a i)
    local n = 0:usize
    for x in (range i)
        n += (bit-at a x)
    n

inline bit-set (a i)
    a | (1 << i)

# convenience
inline node-of-kv (cls key value)
    let new-kv =
        cls.Key-Value-type
            key = key
            value = value
    let new-node =
        cls.Node-type.Key-Value
            Rc.wrap new-kv
    new-node

inline node-of-mb (cls map base)
    let new-mb =
        cls.Map-Base-type
            map = map
            base = base
    let new-node =
        cls.Node-type.Map-Base
            Rc.wrap new-mb
    new-node

typedef Hamt < Struct

# not using because it doesn't correctly memo default arg values
#let decorate-inline... = decorate-fn

@@ memo
inline gen-type (element-type key-type hash-function hash-type map-type index-width)
    # element-type is contents of the map
    # key-type is the key into the map
    # hash-function is itself
    # hash-type is the output of the hash function
    # map-type is the bitmap
    # index-width is also related to the bitmap
    # e.g. if map-type is u32, then index-width is 5, so that 2^5<=32
    # a relationship between hash-type and index-width determines the maximum tree depth
    # e.g. if hash-type is u32 and index-width is 5, then ceil(32/5)=7 is the maximum depth

    let index-length = (1 << index-width)
    static-assert
        ((sizeof map-type) * 8) >= index-length
        "map-type is too short"

    enum Node-type

    let value-type = element-type
    struct Key-Value-type
        key   : key-type
        value : value-type

    # TODO: growing (up to index-length), not fixed
    # investigate mem usage
    let base-type = (FixedArray Node-type index-length)
    struct Map-Base-type
        map  : map-type
        base : base-type

    enum Node-type
        Key-Value : (Rc Key-Value-type)
        Map-Base  : (Rc Map-Base-type)

    let root-e-type = (Option Node-type)
    let root-type = (FixedArray root-e-type index-length)

    inline mask-bits (n x)
        let mask =
            (1 << n) - 1
        x & mask

    inline index-at-depth (index depth)
        let index-shift =
            index >> (index-width * depth)
        mask-bits index-width index-shift

    struct
        .. "<Hamt " (tostring element-type) ">"
        \ < Hamt
        root : root-type
        let key-type hash-function index-length base-type root-e-type root-type
        let Node-type Key-Value-type Map-Base-type
        let index-at-depth

inline... gen-type-2 (element-type, key-type = u32, hash-function = id-hash, hash-type = u32, map-type = u32, index-width : usize = 5)
    gen-type element-type key-type hash-function hash-type map-type index-width

typedef+ Hamt
    # TYPECALL
    inline __typecall (cls etc...)
        static-if (cls == this-type)
            gen-type-2 etc...
        else
            local root = (cls.root-type)
            # fill with none
            for i in (range cls.index-length)
                'append root (cls.root-e-type)
            Struct.__typecall cls root

    # AT, GET
    inline __@ (self index)

    # INSERT, SET
    # TODO: is there a proper method name for this?
    # the gist: hash `index' to get a sequence of tree-indices
    # index the root; if empty, insert kv; if kv and identical k, replace;
    # otherwise replace with a new mb
    # the mb contains the old data and new data merged
    # the merging is done by descending mbs until getting either kv or nothing
    # if nothing, ez just insert but be careful to order it properly
    # if kv and identical, replace;
    # if not identical, hash the old k, and make new mbs deep enough to no longer collide
    # then insert old and then new
    # let's recap:
    # hash index
    # hash-index @ 0
    # get (root @ hash-index-0)
    # l: if empty then insert kv
    # if kv and old-k == new-k then replace kv
    # if kv then new mb and resolve conflict
    # if mb then hash-index @ n and goto l
    # TODO: actually handle hash collisions with hash-depth
    inline set (self index element)
        let cls = (typeof self)
        let root = self.root
        let key = (index as cls.key-type)
        let key-hash = (cls.hash-function 0 key)
        let key-hash-truncated = (cls.index-at-depth key-hash 0)
        let old-entry = (root @ key-hash-truncated)
        let new-entry =
            do
                fn resolve-conflict (new-key new-key-hash old-key old-key-hash tree-depth hash-depth new-value old-value) (returning (uniqueof cls.Node-type -1))
                    let new-i = (cls.index-at-depth new-key-hash tree-depth)
                    let old-i = (cls.index-at-depth old-key-hash tree-depth)
                    if (new-i == old-i)
                        let new-node =
                            this-function new-key new-key-hash old-key old-key-hash (tree-depth + 1) hash-depth new-value old-value
                        let map =
                            (bit-set 0 new-i) as cls.key-type
                        local base = (cls.base-type)
                        'append base new-node
                        node-of-mb cls map base
                    else
                        let new-node = (node-of-kv cls new-key new-value)
                        let old-node = (node-of-kv cls old-key old-value)
                        let map =
                            (bit-set (bit-set 0 new-i) old-i) as cls.key-type
                        local base = (cls.base-type)
                        if (new-i < old-i)
                            'append base new-node
                            'append base old-node
                        else
                            'append base old-node
                            'append base new-node
                        node-of-mb cls map base
                fn set-inner (old-node new-key new-key-hash tree-depth hash-depth new-value) (returning (uniqueof cls.Node-type -1)) (raising Error)
                    dispatch old-node
                    case Key-Value (kv)
                        let old-key = kv.key
                        if (new-key == old-key)
                            node-of-kv cls new-key new-value
                        else
                            let old-key-hash = (cls.hash-function hash-depth old-key)
                            let old-value = kv.value
                            resolve-conflict new-key new-key-hash old-key old-key-hash tree-depth hash-depth new-value old-value
                    case Map-Base (mb)
                        let i = (cls.index-at-depth new-key-hash tree-depth)
                        let old-map = mb.map
                        let old-base = mb.base
                        let new-map =
                            (bit-set old-map i) as cls.key-type # idempotent, hopefully
                        local new-base = (copy old-base)
                        let base-i = (ctpop old-map i)
                        if ((bit-at old-map i) == 0)
                            let new-inner-entry = (node-of-kv cls new-key new-value)
                            'insert new-base new-inner-entry base-i
                        else
                            let old-inner-entry = (old-base @ base-i)
                            let new-inner-entry =
                                this-function old-inner-entry new-key new-key-hash (tree-depth + 1) hash-depth new-value
                            (new-base @ base-i) = new-inner-entry
                        node-of-mb cls new-map new-base
                    default
                        error "wtf default1?"
                dispatch old-entry
                case None ()
                    node-of-kv cls key element
                case Some (old)
                    set-inner old key key-hash 1 0:usize element
                default
                    error "wtf default!?"
        let new-root = (copy root)
        (new-root @ key-hash-truncated) = (cls.root-e-type new-entry)
        Struct.__typecall cls new-root

    # REPR
    # TODO: convert this into a generic iterator
    # lololol this is so bad
    inline __repr (self)
        let cls = (typeof self)
        let root = self.root
        local s = S"{ "
        fn print-my-ass (node) (returning (uniqueof String -1))
            local s = S""
            dispatch node
            case Key-Value (kv)
                s ..= (repr kv.key)
                s ..= " = "
                s ..= (repr kv.value)
            case Map-Base (mb)
                s ..= "["
                s ..= (tostring mb.map)
                s ..= "] { "
                for i in (range (countof mb.base))
                    s ..= (tostring i)
                    s ..= ": "
                    s ..= (this-function (mb.base @ i))
                    s ..= "; "
                s ..= "}"
            default
                ;
            s
        for i in (range cls.index-length)
            dispatch (root @ i)
            case Some (x)
                s ..= (repr x)
                s ..= ": "
                s ..= (print-my-ass x)
                s ..= "; "
            default
                ;
        s ..= "}"
        s

    # debugging, might remove later
    inline print-reftree (self)

do
    let Hamt
    locals;
