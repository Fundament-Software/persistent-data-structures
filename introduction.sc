using import enum

let greeting =
    do
        let name = "Erry"
        "Hello, " .. name
print greeting

fn id (x)
    x
print (id greeting)

enum mylist
    MyNil
    MyCons
