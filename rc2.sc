using import Rc
let data1 = (Rc.wrap 42)
let data2 = (copy data1)
let data3 = (Rc.wrap (copy data2))
print (data1 == data2)
print (data1 == data3)
print (data2 == data3)
# :<<<<
let data4 =
    do
        let rct = (typeof data1)
        Rc.wrap (data1 as rct.Type)
print (data1 == data4)
print data1
print data4
;
