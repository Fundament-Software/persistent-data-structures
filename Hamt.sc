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
        let hash-function index-length root-e-type root-type
        let index-at-depth

inline... gen-type-2 (element-type, key-type = u32, hash-function = id, hash-type = u32, map-type = u32, index-width : usize = 5)
    gen-type element-type key-type hash-function hash-type map-type index-width

typedef+ Hamt
    inline __typecall (cls etc...)
        static-if (cls == this-type)
            gen-type-2 etc...
        else
            local root = (cls.root-type)
            # fill with none
            for i in (range cls.index-length)
                'append root (cls.root-e-type)
            Struct.__typecall cls root

    inline __@ (self index)

    # TODO: is there a proper method name for this?
    inline set (self index element)
        let cls = (typeof self)
        let root = self.root
        let key-hash = (cls.hash-function index)
        let key-hash-truncated = (cls.index-at-depth key-hash 0)
        let entry = (root @ key-hash-truncated)
        dispatch entry
        case None ()
            print "none agon"
        case Some (x)
            print "some buddy"
        default
            assert false "wtf default!?"
        Struct.__typecall cls (copy root)

    inline __repr (self)

    # debugging, might remove later
    inline print-reftree (self)

do
    let Hamt
    locals;
