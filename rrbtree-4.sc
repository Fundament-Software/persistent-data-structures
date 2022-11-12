using import Array
using import enum
using import Option
using import Rc
using import struct

let block-width = 5
let node-arity = (2 ** block-width)
let block-mask = (node-arity - 1)

type RrbVector < Struct
    inline gen-rrb-vector-type (cls element-type)
        let node-type = (Rc this-type)
        #let node-type = element-type
        let leaf-type = element-type

        report (typeof leaf-type)
        report (typeof node-type)

        let NodeOrLeaf =
            enum
                ..
                    "<NodeOrLeaf "
                    tostring node-type
                    ", "
                    tostring leaf-type
                    ">"

                Node : node-type
                Leaf : leaf-type

        struct
            ..
                "<RrbVector "
                tostring element-type
                ">"
            \ < this-type

            #length : usize
            depth  : usize
            #tree   : (FixedArray NodeOrLeaf node-arity)
            tree   : (FixedArray element-type node-arity)
            ttttt  : node-type

    inline __typecall (cls opt)
        static-if (cls == this-type)
            let element-type = opt
            gen-rrb-vector-type this-type element-type
        else
            let vec = (Struct.__typecall cls)
            vec

    #fn appended (self value)
        local new_rrb_vector = ('new (typeof self))
        new_rrb_vector.depth = self.depth
        new_rrb_vector.tree = (copy self.tree)
        'append new_rrb_vector.tree value
        new_rrb_vector

    #fn get (self index)
        fn get-radix (idx node level)
            if (level == 0)
                node @ (idx & block-mask)
            else
                3
        get-radix index self.tree self.depth

    #fn get (self index)
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

do
    let rrbvector-i32 = (RrbVector i32)
    report rrbvector-i32
    local myvector = (rrbvector-i32)
    #'append myvector.tree 42
    report (typeof myvector)
    report (countof myvector.tree)
    #local myvector2 = ('appended myvector 42)
    #report (typeof myvector2)
    #report (countof myvector2.tree)
    #report ('get myvector2 0)
    ;

# testy testy
#do
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
    print (countof myvector.tree)
    'append myvector.tree 1
    print (countof myvector.tree)
    ;

;
