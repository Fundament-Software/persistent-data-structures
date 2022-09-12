using import struct

struct MyStruct
    thing : i32

    fn increment (self)
        self.thing += 1

do
    local x =
        MyStruct
            thing = 42
    print x.thing
    'increment x
    print x.thing
