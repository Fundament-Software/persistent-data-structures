sugar unwrap (var body...)
    qq
        do
            dispatch [var]
            case
                unquote-splice body...
            default
                assert false "unwrap failed, PANIC!!!"
                unreachable;

# TODO: more params in brcase?
sugar let-unwrap (name var brcase)
    qq
        let [name] =
            unwrap [var] [brcase] (x) x

do
    let unwrap let-unwrap
    locals;
