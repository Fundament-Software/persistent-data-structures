using import Array
using import enum
using import Rc
using import struct

inline blint (thing)
    print "#### BLINT"
    print (typeof thing)
    print (tostring thing)

do
    let mytype = i32
    blint mytype
    let mydata = (mytype)
    blint mydata
    let myrctype = (Rc mytype)
    blint myrctype
    let myrcdata = (myrctype)
    blint myrcdata
    let myarraytype = (FixedArray myrctype 32)
    blint myarraytype
    local myarraydata = (myarraytype)
    blint myarraydata
    'append myarraydata myrcdata
    blint myarraydata
    ;

enum Aaaa
    Left : i32
    Right : (Rc this-type)

do
    let mytype = Aaaa
    blint mytype
    #let mydata = (mytype)
    #blint mydata
    # enum has no constructor
    let myarraytype = (FixedArray mytype 32)
    blint myarraytype
    local myarraydata = (myarraytype)
    blint myarraydata
    ;

struct Bbbb
    Top : i32
    Bottom : (Rc this-type)

do
    let mytype = Bbbb
    blint mytype
    #let mydata = (mytype)
    #blint mydata
    # infinite recursion in constructor
    ;
