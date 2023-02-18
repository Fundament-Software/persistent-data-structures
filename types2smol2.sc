@@ memoize
inline gen-bit-ops (block-width)
    let node-arity = (2 ** block-width)
    locals;

let bit-ops = (gen-bit-ops 2)
run-stage;
bit-ops.node-arity
;
