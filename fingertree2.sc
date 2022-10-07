using import enum
using import Rc

# NOTE
# non-regular recursive types are hard in scopes
# so this tree type lacks enforcement of constant depth
# similar limitations apply to the other types too
# TODO
# y'know, i'm writing this boilerplate so many times
# i might want to make a macro (sugar?) for "enum with type parameters"
type Tree23 < Enum
    inline __typecall (cls element-type)
        enum
            ..
                "<Tree23 "
                tostring element-type
                ">"
            \ < this-type

            Leaf  : element-type
            Node2 : (Rc this-type) (Rc this-type)
            Node3 : (Rc this-type) (Rc this-type) (Rc this-type)

# testy testy
do
    print "Tree23 start"
    let tree-i32 = (Tree23 i32)
    report tree-i32
    let mytree = (tree-i32.Leaf 0)
    report mytree
    let mytree2 =
        tree-i32.Node2
            Rc.wrap (copy mytree)
            Rc.wrap (copy mytree)
    report mytree2
    print "Tree23 end"
    ;
# yay

type Digit < Enum
    inline __typecall (cls element-type)
        enum
            ..
                "<Digit "
                tostring element-type
                ">"
            \ < this-type

            One   : element-type
            Two   : element-type element-type
            Three : element-type element-type element-type
            Four  : element-type element-type element-type element-type

type FingerTree < Enum
    inline __typecall (cls element-type)
        enum
            ..
                "<FingerTree "
                tostring element-type
                ">"
            \ < this-type

            Empty
            Single : (Tree23 element-type)
            Deep   : (Digit (Tree23 element-type))

# load-bearing semicolon
;
