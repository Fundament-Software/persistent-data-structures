using import Option
using import Rc

let Hamt = (import .Hamt)
using import .run
using import .unwrap

fn... set (self : Hamt)
    fn set-inner (old-entry)
        returning void
        dispatch old-entry
        case None () ()
        case Some (old-node)
            dispatch old-node
            case Key-Value (old-kv) ()
            case Map-Base (old-mb)
                this-function (old-mb @ 0)
            default ()
        default ()
    set-inner (self.root @ 0)

run
    let my-hamt = ((Hamt i32))
    set my-hamt
