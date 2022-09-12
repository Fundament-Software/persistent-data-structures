import enum
import Rc

#type NormalTree
    inline gen-normal-tree-type (element-type)
        let gib = this-type

        enum
            .. "<NormalTree " (tostring element-type) ">"
            Leaf : element-type
            Node : (Rc gib)

    inline __typecall (cls element-type)
        gen-normal-tree-type element-type

do
    let rcthing = (Rc.Rc i32)
    let idk = (rcthing)
