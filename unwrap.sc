sugar unwrap (var body...)
    qq
        do
            dispatch [var]
            case
                unquote-splice body...
            default
                unreachable;

sugar let-unwrap (name var brcase param)
    qq
        let [name] =
            unwrap [var] [brcase] ([param]) [param]

do
    let unwrap let-unwrap
    locals;
