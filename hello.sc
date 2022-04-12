print "test"
    "test2" # interesting how
              scopes works
    "test3"

let test = """"woah
               this
               is
print test """"cool

let test2 = (.. "wait" "w" "t" "f")
print test2

fn fib (n)
    loop (a b = 0 1)
        if (b < n)
            print b
            repeat b (a + b)
        else
            break b

fib 2000
