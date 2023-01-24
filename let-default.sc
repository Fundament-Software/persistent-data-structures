sugar let-default (body...)
    let syms vals =
        loop (syms rest = '() body...)
            sugar-match rest
            case ('= rest...)
                break ('reverse rest)
            case (sym rest...)


do
    let let-default
    locals;
