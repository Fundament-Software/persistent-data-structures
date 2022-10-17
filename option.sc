using import Option

do
    # making an Option<i32>
    # this makes sense, opt-type is Option<i32> and typeof opt-type is type
    let opt-type = (Option i32)
    report opt-type
    report (typeof opt-type)
    # print slightly usefully
    fn do-thing (thing)
        dispatch thing
        case Some (x)
            print (tostring x)
        case None ()
            print "None"
        default
            print "this needs to be improved"
    # trying to make a None data of Option<i32>
    # instead i make a new type???
    #let opt-none-type = opt-type.None
    #report opt-none-type
    #report (typeof opt-none-type)
    # 100% lost
    #local opt-none : opt-none-type
    #report opt-none
    #report (typeof opt-none)
    # but i can make a Some(42) of Option<i32> so that's good
    # opt-some is (42 as Option) and typeof opt-some is Option<i32>
    local forty-two = 42
    local opt-some =
        Option.wrap forty-two
    report opt-some
    report (typeof opt-some)
    do-thing opt-some
    # i can do this too
    local opt-some-other = (opt-type forty-two)
    report opt-some-other
    report (typeof opt-some-other)
    do-thing opt-some-other
    # wait
    # aaaaaaa
    local opt-none = (opt-type)
    report opt-none
    report (typeof opt-none)
    do-thing opt-none
    ;

;
