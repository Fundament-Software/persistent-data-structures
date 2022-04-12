loop (x = 2)
    if (x < 10000)
        let prime =
            loop (i = 2)
                if (i * i <= x)
                    if (x % i == 0)
                        break false
                    else
                        repeat (i + 1)
                else
                    break true
        if prime
            print x
        repeat (x + 1)
    else
        break true
