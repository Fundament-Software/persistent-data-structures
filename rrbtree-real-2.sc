using import enum
using import struct

# TODO type parameter A
struct RRB
    length : usize
    middle-level : usize
    outer-f : PoolRef
    inner-f : PoolRef
    middle : Ref
    outer-b : PoolRef
    inner-b : PoolRef

# TODO type parameter A
enum VectorInner
    Inline : RRBPool InlineArray
    Single : RRBPool PoolRef
    Full : RRBPool RRB

# TODO type parameter A
struct Vector
    vector : VectorInner

    fn pool (self)
        dispatch self.vector
        case Inline (pool _)
            pool
        case Single (pool _)
            pool
        case Full (pool _)
            pool

    fn needs-promotion (self)
        # TODO refactor this?
        dispatch self.vector
        case Inline (_ chunk)
            'is-full chunk
        case Single (_ chunk)
            'is-full chunk
        default
            false

    fn promote-inline (self)
        # TODO refactor this?
        dispatch self.vector
        case Inline (pool chunk)
            self.vector =
                Single
                    'clone pool
                    'new PoolRef
                        pool.value-pool
                        # TODO into???
                        chunk
        default
            ;

    fn promote-front (self)
        self.vector =
            dispatch self.vector
            case Inline (pool chunk)
                # literally a copypaste of earlier
                Single
                    'clone pool
                    'new PoolRef
                        pool.value-pool
                        chunk
            case Single (pool chunk)
                Full
                    'clone pool
                    Rrb
                        length = ('len chunk)
                        middle-level = 0
                        outer-f = ('default PoolRef pool.value-pool)
                        inner-f = chunk
                        middle = ('new Ref ('new Node))
                        inner-b = ('default PoolRef pool.value-pool)
                        outer-b = ('default PoolRef pool.value-pool)
            case Full (_ _)
                self.vector

    fn promote-back (self)
        self.vector =
            dispatch self.vector
            case Inline (pool chunk)
                # literally a copypaste of earlier
                Single
                    'clone pool
                    'new PoolRef
                        pool.value-pool
                        chunk
            case Single (pool chunk)
                Full
                    'clone pool
                    Rrb
                        length = ('len chunk)
                        middle-level = 0
                        outer-f = ('default PoolRef pool.value-pool)
                        inner-f = ('default PoolRef pool.value-pool)
                        middle = ('new Ref ('new Node))
                        inner-b = chunk
                        outer-b = ('default PoolRef pool.value-pool)
            case Full (_ _)
                self.vector

    # TODO use __typecall instead of new
    fn new ()
        this-type
            vector =
                Inline
                    'default RRBPool
                    'new InlineArray

    fn with-pool (pool)
        this-type
            vector =
                Inline
                    'clone pool
                    'new InlineArray

    fn len (self)
        dispatch self.vector
        case Inline (_ chunk)
            'len chunk
        case Single (_ chunk)
            'len chunk
        case Full (_ tree)
            tree.length

    fn is-empty (self)
        ('len self) == 0

    # TODO convenience function?
    fn is-inline (self)
        dispatch self.vector
        case Inline (_ _)
            true
        default
            false

    # TODO this one confuses me, ask open
    fn ptr-eq (self other)
        fn cmp-chunk (left right)
            ||
                &&
                    'is-empty left
                    'is-empty right
                'ptr-eq PoolRef left right

        if ()

    fn iter (self)
        'new Iter self

    fn iter-mut (self)
        'new IterMut self

    fn leaves (self)
        'new Chunks self

    fn leaves-mut (self)
        'new ChunksMut self

    fn focus (self)
        'new Focus self

    fn focus-mut (self)
        'new FocusMut self

    fn get (self index)
        if (index >= ('len self))
            None
        else
            dispatch self.vector
            case Inline (_ chunk)
                'get chunk index
            case Single (_ chunk)
                'get chunk index
            case Full (_ tree)
                local local_index = index

                if (local_index < ('len tree.outer-f))
                    return
                        Some (@)
                local_index -= ('len tree.outer-f)

                if (local_index < ('len tree.inner-f))
                    return
                        Some (@)
                local_index -= ('len tree.inner-f)

                if (local_index < ('len tree.middle))
                    return
                        Some (@)
                local_index -= ('len tree.middle)

                if (local_index < ('len tree.inner-b))
                    return
                        Some (@)
                local_index -= ('len tree.inner-b)

                Some (@)

    fn get-mut (self index)
        if (index >= ('len self))
            None
        else
            dispatch self.vector
            case Inline (_ chunk)
                'get-mut chunk index
            case Single (_ chunk)
                'get-mut ('make-mut PoolRef pool.value-pool chunk) index
            case Full (_ tree)
                local local_index = index

                if (local_index < ('len tree.outer-f))
                    let outer-f =
                        'make-mut PoolRef pool.value-pool tree.outer-f
                    return
                        Some (@)
                local_index -= ('len tree.outer-f)

                if (local_index < ('len tree.inner-f))
                    let inner-f =
                        'make-mut PoolRef pool.value-pool tree.inner-f
                    return
                        Some (@)
                local_index -= ('len tree.inner-f)

                if (local_index < ('len tree.middle))
                    let middle =
                        'make-mut PoolRef pool.value-pool tree.middle
                    return
                        Some (@)
                local_index -= ('len tree.middle)

                if (local_index < ('len tree.inner-b))
                    let inner-b =
                        'make-mut PoolRef pool.value-pool tree.inner-b
                    return
                        Some (@)
                local_index -= ('len tree.inner-b)

                let outer-b =
                    'make-mut PoolRef pool.value-pool tree.outer-b
                Some (@)

    fn front (self)
        'get self 0

    fn front-mut (self)
        'get-mut self 0

    let head = front

    fn back (self)
        if ('is-empty self)
            None
        else
            let len = ('len self)
            'get self (len - 1)

    fn back-mut (self)
        if ('is-empty self)
            None
        else
            let len = ('len self)
            'get-mut self (len - 1)

    let last = back

    fn index_of (self value)

    fn contains (self value)

    fn clear (self)
        if (! ('is-empty self))
            self.vector =
                Inline
                    'clone ('pool self)
                    'new InlineArray
