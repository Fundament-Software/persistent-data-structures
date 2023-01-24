inline static-or (var def)
    static-if (none? var)
        def
    else
        var

do
    let static-or
    locals;
