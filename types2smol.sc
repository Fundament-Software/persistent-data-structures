@@ memo
inline some-scope (thing)
    locals;

type my-type
    @@ type-factory
    inline gen-my-type (thing)
        some-scope thing
        u32

    inline __typecall (cls)
        gen-my-type 0

my-type;
;
