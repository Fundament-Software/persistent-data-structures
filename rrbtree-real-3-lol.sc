using import Array
using import enum
using import Option
using import Rc
using import struct

let block-width = 5
let node-arity = (2 ** block-width)
let block-mask = (node-arity - 1)

type RrbVector < Struct
    inline __typecall (cls element-type)
        static-if (cls == this-type)
            'new-type this-type element-type
        else
            'new cls

    inline new-type (cls element-type)
        # TODO
        # garbage
        #inline new-tree (node-type leaf-type)
            enum NodeOrLeaf
                Node : node-type
                Leaf : leaf-type
            FixedArray NodeOrLeaf node-arity

        struct
            ..
                "<RrbVector "
                tostring element-type
                ">"
            \ < this-type

            length : usize
            depth  : usize
            #tree   : (new-tree (Rc this-type) element-type)
            tree   : (FixedArray element-type node-arity)

    inline new (cls)
        Struct.__typecall cls
            depth = 1

    fn get (self index)
        #fn get-radix (idx node level)
            if (level == 0)
                node @ (idx & block-mask)
            else
                let index-in-level = ((idx >> (level * block-width)) & block-mask)
                this-function idx (node @ index-in-level) (level - 1)
        #get-radix index self.tree self.depth
        # TODO
        # how do i make it not i32
        let the-type = (Option i32)
        if (index >= (countof self.tree))
            (the-type) # None
        else
            # Some
            the-type
                self.tree @ index

# testy testy
do
    let rrbvector-i32 = (RrbVector i32)
    report rrbvector-i32
    local myvector = (rrbvector-i32)
    #report myvector # rip
    report (typeof myvector)
    let mything = ('get myvector 1)
    report mything
    report (typeof mything)
    dispatch mything
    case None ()
        print "none"
    case Some ()
        print "some"
    default
        print "wtf"
    ;

;
