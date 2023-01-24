sugar unwrap (var body...)
    qq
        do
            dispatch [var]
            case
                unquote-splice body...
            default
                unreachable;

do
    let unwrap
    locals;
