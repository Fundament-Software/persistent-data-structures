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

inline id (x)
    x

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
        let key-type hash-function index-length root-e-type root-type
        let Node-type Key-Value-type Map-Base-type
        let index-at-depth

inline... gen-type-2 (element-type, key-type = u32, hash-function = id, hash-type = u32, map-type = u32, index-width : usize = 5)
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
    inline set (self index element)
        let cls = (typeof self)
        let root = self.root
        let key = (index as cls.key-type)
        let key-hash = (cls.hash-function key)
        let key-hash-truncated = (cls.index-at-depth key-hash 0)
        let entry = (root @ key-hash-truncated)
        let new-root = (copy root)
        dispatch entry
        case None ()
            let new-kv =
                cls.Key-Value-type
                    key = key
                    value = element
            let new-node =
                cls.Node-type.Key-Value
                    Rc.wrap new-kv
            (new-root @ key-hash-truncated) = (cls.root-e-type new-node)
        #case Some (x)
            fn set-inner (node key key-hash depth element) (returning (uniqueof cls.Node-type -1))
                let i = (cls.index-at-depth key-hash depth)
                dispatch node
                case Key-Value (kv)
                case Map-Base (mb)
                default
                    assert false "wtf default1?"
            let new-node = (set-inner x key 1 element)
            (new-root @ key-hash-truncated) = (cls.root-e-type new-node)
        # FIXME: stub to test quickly
        case Some (x)
            let new-kv =
                cls.Key-Value-type
                    key = key
                    value = element
            let new-node =
                cls.Node-type.Key-Value
                    Rc.wrap new-kv
            (new-root @ key-hash-truncated) = (cls.root-e-type new-node)
        default
            assert false "wtf default!?"
        Struct.__typecall cls new-root

    inline __repr (self)

    # debugging, might remove later
    inline print-reftree (self)

do
    let Hamt
    locals;
