using import Array
using import enum
using import Rc
using import struct

using import .run

inline bleet (text)
    print ("###### " .. text)

inline blint (thing)
    print "#### BLINT"
    print (typeof thing)
    print (tostring thing)

inline blint2 (thing)
    print "#### BLINT"
    print (typeof thing)
    #print (tostring thing)

# run
#     bleet "i32"
#     let mytype = i32
#     blint mytype
#     let mydata = (mytype)
#     blint mydata
#     let myrctype = (Rc mytype)
#     blint myrctype
#     let myrcdata = (myrctype)
#     blint myrcdata
#     let myarraytype = (FixedArray myrctype 32)
#     blint myarraytype
#     local myarraydata = (myarraytype)
#     blint myarraydata
#     'append myarraydata myrcdata
#     blint myarraydata

# enum Aaaa
#     Left : i32
#     Right : (FixedArray (Rc this-type) 32)

# run
#     bleet "enum"
#     let mytype = Aaaa
#     blint mytype
#     #let mydata = (mytype)
#     #blint mydata
#     # enum has no constructor

# struct Bbbb
#     Top : i32
#     Bottom : (FixedArray (Rc this-type) 32)

# run
#     bleet "struct"
#     let mytype = Bbbb
#     blint mytype
#     let mydata = (mytype)
#     blint2 mydata

# type Cccc
#     inline __typecall (cls)
#         FixedArray (Rc this-type) 32

# run
#     bleet "typecall"
#     let mytype = Cccc
#     blint mytype
#     let mytype2 = (mytype)
#     blint mytype2
#     let mydata = (mytype2)
#     blint mydata

# type Dddd < Struct
#     inline __typecall (cls)
#         static-if (cls == this-type)
#             struct "Dddd-inner" < this-type
#                 one : i32
#                 two : (FixedArray (Rc this-type) 32)
#         else
#             Struct.__typecall cls

# run
#     bleet "typecall struct"
#     let mytype = Dddd
#     blint mytype
#     let mytype2 = (mytype)
#     blint mytype2
#     let mydata = (mytype2)
#     blint2 mydata

type Eeee < Struct
    inline __typecall (cls)
        static-if (cls == this-type)
            struct "Eeee-inner" < this-type
                one : i32
                two : (FixedArray (Rc this-type) 32)
        else
            Struct.__typecall cls

run
    bleet "typecall struct"
    let mytype = Eeee
    blint mytype
    let mytype2 = (mytype)
    blint mytype2
    let mydata = (mytype2)
    blint2 mydata

type Ffff < Struct
    inline __typecall (cls)
        static-if (cls == this-type)
            struct "Ffff-inner" < this-type
                one : i32
                two : (FixedArray (Rc cls) 32)
        else
            Struct.__typecall cls

run
    bleet "typecall struct with cls"
    let mytype = Ffff
    blint mytype
    let mytype2 = (mytype)
    blint mytype2
    # BUG issue is here
    #let mydata = (mytype2)
    #blint2 mydata

# type Gggg < Struct
# inline gen-gggg ()
#     let how = (FixedArray (Rc Gggg) 32)
#     enum Gggg-inner-enum
#         Left : i32
#         Right : how
#     struct "Gggg-inner-struct" < Gggg
#         let how = how
#         let what = Gggg-inner-enum
#         one : i32
#         two : Gggg-inner-enum
# type+ Gggg
#     inline __typecall (cls one two)
#         static-if (cls == this-type)
#             gen-gggg;
#         else
#             Struct.__typecall cls
#                 one = one
#                 two = two

# run
#     bleet "typecall external enum-struct with type+"
#     let mytype = Gggg
#     blint mytype
#     let mytype2 = (mytype)
#     blint mytype2
#     let mytype3 = mytype2.how
#     blint mytype3
#     let mydata3 = (mytype3)
#     blint mydata3
#     let mytype4 = mytype2.what
#     blint mytype4
#     let mydata4 = (mytype4.Right mydata3)
#     blint mydata4
#     let mydata = (mytype2 0 mydata4)
#     blint2 mydata

# type Hhhh < Struct
# inline gen-hhhh (element-type)
#     let node-type = (FixedArray (Rc Hhhh) 32)
#     enum Hhhh-inner-enum
#         Leaf : element-type
#         Node : node-type
#     struct "Hhhh-inner-struct" < Hhhh
#         let node-type = node-type
#         let tree-type = Hhhh-inner-enum
#         depth : i32
#         tree : Hhhh-inner-enum
# type+ Hhhh
#     inline __typecall (cls element-type)
#         static-if (cls == this-type)
#             gen-hhhh element-type
#         else
#             let node-type = cls.node-type
#             let tree-type = cls.tree-type
#             let empty-node = (node-type)
#             let empty-tree = (tree-type.Node empty-node)
#             Struct.__typecall cls
#                 depth = 1
#                 tree = empty-tree

# run
#     let mytype = Hhhh
#     blint mytype
#     let mytype2 = (mytype i32)
#     blint mytype2
#     let mydata = (mytype2)
#     blint mydata
