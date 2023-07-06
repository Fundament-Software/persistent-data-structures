let BsArray = (import .BsArray)
using import .unwrap
let mycls = (BsArray i32)
local mydata = (mycls)
print (tostring (mydata @ 0))
print (tostring (mydata @ 1))
print (tostring (mydata @ 31))
print (repr mydata)
'set mydata 0 42
'set mydata 31 69
print (tostring (mydata @ 0))
print (tostring (mydata @ 1))
print (tostring (mydata @ 31))
let-unwrap zero (mydata @ 0) Some
let-unwrap thirtyone (mydata @ 31) Some
print (tostring zero)
print (tostring thirtyone)
print (repr mydata)
'set mydata 1 420
'set mydata 31 1337
print (tostring (mydata @ 0))
print (tostring (mydata @ 1))
print (tostring (mydata @ 31))
let-unwrap zero (mydata @ 0) Some
let-unwrap one (mydata @ 1) Some
let-unwrap thirtyone (mydata @ 31) Some
print (tostring zero)
print (tostring one)
print (tostring thirtyone)
print (repr mydata)
;
