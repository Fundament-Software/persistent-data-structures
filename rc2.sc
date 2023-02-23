using import Rc
let data1 = (Rc.wrap 42)
let data2 = (Rc.wrap 42)
let data3 = (copy data1)
let data4 = (copy data2)
print
    ..
        "data1 == data2: "
        repr (data1 == data2)
print
    ..
        "data1 == data3: "
        repr (data1 == data3)
print
    ..
        "data1 == data4: "
        repr (data1 == data4)
print
    ..
        "data2 == data3: "
        repr (data2 == data3)
print
    ..
        "data2 == data4: "
        repr (data2 == data4)
print
    ..
        "data3 == data4: "
        repr (data3 == data4)
;
