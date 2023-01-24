using import Array
using import enum
using import .run

#enum rrbtree
#let rrbtree-data-node-type =
    FixedArray i32 32
#let rrbtree-pointer-node-type =
    FixedArray rrbtree 32
#enum rrbtree
    data-node    : rrbtree-data-node-type
    pointer-node : rrbtree-pointer-node-type

#run
    let my-thing =
        rrbtree.data-node
            rrbtree-data-node-type;
    local my-thing-2-p =
        rrbtree-pointer-node-type;
    'append my-thing-2-p my-thing
    let my-thing-2 =
        rrbtree.pointer-node my-thing-2-p
    let my-thing-3 =
        rrbtree.pointer-node
            do
                local my-thing-3-p =
                    rrbtree-pointer-node-type;
                'append my-thing-3-p my-thing-2
                my-thing-3-p
    print my-thing-3

type rrbtree-any
    inline __typecall (element-type)
        #enum rrbtree-2
        let rrbtree-data-node-type =
            FixedArray element-type 32
        #let rrbtree-pointer-node-type =
            FixedArray rrbtree-2 32
        enum rrbtree-2
            data-node    : rrbtree-data-node-type
            #pointer-node : rrbtree-pointer-node-type
            let rrbtree-data-node-type
            #let rrbtree-pointer-node-type
        rrbtree-2

run
    let rrbtree-i32 =
        rrbtree-any i32
    let my-thing =
        rrbtree-i32.data-node
            rrbtree-i32.rrbtree-data-node-type;
    #local my-thing-2-p =
        rrbtree-i32.rrbtree-pointer-node-type;
    #'append my-thing-2-p my-thing
    #let my-thing-2 =
        rrbtree-i32.pointer-node my-thing-2-p
    #let my-thing-3 =
        rrbtree-i32.pointer-node
            do
                local my-thing-3-p =
                    rrbtree-i32.rrbtree-pointer-node-type;
                'append my-thing-3-p my-thing-2
                my-thing-3-p
    #print my-thing-3
