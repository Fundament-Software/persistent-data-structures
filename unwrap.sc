sugar unwrap (var body...)
    qq
        do
            dispatch [var]
            case
                unquote-splice body...
            default
                assert false "unwrap failed, PANIC!!!"
                unreachable;

sugar let-unwrap (name var brcase param)
    qq
        let [name] =
            unwrap [var] [brcase] ([param]) [param]

do
    let unwrap let-unwrap
    locals;
