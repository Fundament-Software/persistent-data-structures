# types needed for gen-type
using import Array
using import Option
using import Rc
using import enum
using import String
using import struct

using import .unwrap
let math = (import .math)
let BsArray = (import .BsArray)

fn... simple-hash (value, hash-depth : usize)
    let t = (typeof value)
    (value + hash-depth) as t

let decorate-inline... = decorate-inline

@@ memo
inline... gen-type (cls : type, Value : type, Key : type, hash-function, Hash : type, Bits : type, bits-width : usize)
    # Value is contents of the map
    # Key is the key into the map, and the input of the hash function
    # hash-function is itself
    # Hash is the output of the hash function
    # Bits is the bitmap
    # bits-width is also related to the bitmap
    # e.g. if Bits is u32, then bits-width is 5, so that 2^5==32
    # a relationship between Hash and bits-width determines the maximum tree depth
    # e.g. if Hash is u32 and bits-width is 5, then ceil(32/5)=7 is the maximum depth

    # TODO: type of function
    #let hash-function = (imply hash-function ???)

    let bits-length = (math.pow2 usize bits-width)
    let bits-size =
        (sizeof Bits) * 8
    static-assert (bits-length <= bits-size) "Bits is too short, or bits-width is too wide"
    # TODO: they should be equal, but it's not breaking if this assertion passes
    # warn when bits-length != bits-size?

    struct
        .. "<Hamt " (tostring Value) ">"
        \ < cls

        let Key Value hash-function Hash Bits bits-length

        enum Node

        struct Key-Value
            key   : Key
            value : Value

        let Map-Base =
            BsArray (Rc Node) Bits

        enum Node
            Key-Value : Key-Value
            Map-Base  : Map-Base
            fn... __repr (self : this-type)
                local s = S""
                dispatch self
                case Key-Value (kv)
                    s ..= "Key-Value: "
                    s ..= (repr kv.key)
                    s ..= " = "
                    s ..= (repr kv.value)
                case Map-Base (mb)
                    s ..= "Map-Base: "
                    s ..= (repr mb)
                default
                    ;
                s

        let Root-Element =
            Option (Rc Node)
        let Root = (FixedArray Root-Element bits-length)

        inline... get-index (hash : Hash, depth : usize)
            let hash-shift =
                math.shr-fix hash (bits-width * depth)
            math.mask-bits bits-width hash-shift

        root : Root

# @@memo isn't smart enough to figure that omitting optional arguments is the same as explicitly passing the default values.
inline... gen-type-defaults (cls : type, Value : type, Key : type = u32, hash-function = simple-hash, Hash : type = u32, Bits : type = u32, bits-width : usize = 5)
    gen-type cls Value Key hash-function Hash Bits bits-width

inline... gen-value-with (cls : type, root)
    let root = (imply root cls.Root)
    Struct.__typecall cls root

# TODO: create with initial data
inline... gen-value (cls : type)
    local root = (cls.Root)
    # fill with none
    for i in (range cls.Root.Capacity)
        'append root (cls.Root-Element)
    gen-value-with cls root

typedef Hamt < Struct
    # TYPECALL
    inline... __typecall (cls : type, etc...)
        static-if (cls == this-type)
            gen-type-defaults cls etc...
        else
            gen-value cls etc...

    # AT, GET
    fn... __@ (self : this-type, key)
        let cls = (typeof self)
        let key = (imply key cls.Key)
        let root = self.root
        let hash = (cls.hash-function key 0)

    # INSERT, SET
    # TODO: is there a magic name for ``(self @ key) = value``?
    # the gist: hash `key' to get a sequence of tree-indices
    # index the root; if empty, insert kv; if kv and identical k, replace;
    # otherwise replace with a new mb
    # the mb contains the old data and new data merged
    # the merging is done by descending mbs until getting either kv or nothing
    # if nothing, ez just insert but be careful to order it properly
    # if kv and identical, replace;
    # if not identical, hash the old k, and make new mbs deep enough to no longer collide
    # then insert old and then new
    # let's recap:
    # hash = hash key
    # index = hash @ 0
    # get (root @ index)
    # l: if empty then insert kv
    # if kv and old-k == new-k then replace kv
    # if kv then new mb and resolve conflict
    # if mb then hash @ n and goto l
    # TODO: actually handle hash collisions with hash-depth
    fn... set (self : this-type, key, value)
        let cls = (typeof self)
        let key = (imply key cls.Key)
        let value = (imply value cls.Value)
        let root = self.root
        let node-return =
            uniqueof (Rc cls.Node) -1

        # TODO: much duplication of kv creation
        fn... resolve-conflict (old-node : (Rc cls.Node), old-key : cls.Key, old-hash : cls.Hash, new-key : cls.Key, new-value : cls.Value, new-hash : cls.Hash, tree-depth : usize, hash-depth : usize)
            returning (_: node-return)
            let old-index = (cls.get-index old-hash tree-depth)
            let new-index = (cls.get-index new-hash tree-depth)
            local mb = (cls.Map-Base)
            if (old-index == new-index)
                let extra-node =
                    this-function old-node old-key old-hash new-key new-value new-hash (tree-depth + 1) hash-depth
                'set mb old-index extra-node
            else
                let new-kv =
                    cls.Key-Value
                        key = new-key
                        value = new-value
                let new-node =
                    Rc.wrap (cls.Node.Key-Value new-kv)
                # TODO: optimize order
                'set mb old-index (copy old-node)
                'set mb new-index new-node
            Rc.wrap (cls.Node.Map-Base mb)
        #end fn resolve-conflict
        fn... set-inner (old-entry : (Option (Rc cls.Node)), new-key : cls.Key, new-value : cls.Value, new-hash : cls.Hash, tree-depth : usize, hash-depth : usize)
            returning (_: node-return)
            dispatch old-entry
            case None ()
                let new-kv =
                    cls.Key-Value
                        key = new-key
                        value = new-value
                Rc.wrap (cls.Node.Key-Value new-kv)
            case Some (old-node)
                dispatch old-node
                case Key-Value (old-kv)
                    let old-key = old-kv.key
                    if (old-key == new-key)
                        let new-kv =
                            cls.Key-Value
                                key = new-key
                                value = new-value
                        Rc.wrap (cls.Node.Key-Value new-kv)
                    else
                        let old-hash = (cls.hash-function old-key hash-depth)
                        resolve-conflict old-node old-key old-hash new-key new-value new-hash tree-depth hash-depth
                case Map-Base (old-mb)
                    let new-index = (cls.get-index new-hash tree-depth)
                    let old-entry = (old-mb @ new-index)
                    let new-entry =
                        this-function old-entry new-key new-value new-hash (tree-depth + 1) hash-depth
                    let new-mb = (copy old-mb)
                    'set new-mb new-index new-entry
                    Rc.wrap (cls.Node.Map-Base new-mb)
                default
                    error "set default1?"
            default
                error "set default!?"
        #end fn set-inner

        let new-hash = (cls.hash-function key 0)
        # evident duplication of Map-Base case in set-inner
        # but root is a FixedArray, not a Map-Base
        let new-index = (cls.get-index new-hash 0)
        let old-entry = (root @ new-index)
        let new-entry = (set-inner old-entry key value new-hash 1 0)
        let new-root = (copy root)
        (new-root @ new-index) = (cls.Root-Element new-entry)

        gen-value-with cls new-root

    # REPR, TOSTRING, PRINT
    fn... __repr (self : this-type)
        let cls = (typeof self)
        let root = self.root
        local s = S"{ "
        for i in (range cls.bits-length)
            dispatch (root @ i)
            case Some (x)
                s ..= (repr x)
                s ..= "; "
            default
                ;
        s ..= "}"
        s

    # debugging
    fn... print-reftree (self : this-type)
