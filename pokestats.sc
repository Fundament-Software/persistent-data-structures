using import String
using import enum
using import struct
let cstdlib = (import C.stdlib)

fn ceilsqrt (x)
    local r = 0
    while ((r * r) < x)
        r += 1
    r

fn pad-end (str len)
    let n = (len - (countof str))
    let spaces =
        ;
            fold (s = (String "")) for _ in (range n)
                s .. " "
            \ as string
    str .. spaces

fn printstat (stat)
    print
        ..
            pad-end stat.name 8
            ": "
            tostring ('calcstat stat)
            " (Base "
            tostring stat.base
            ", DV "
            tostring stat.dv
            ", EV "
            tostring stat.ev
            ")"

fn calcpre (stat)
    let gene = ((stat.base + stat.dv) * 2)
    let effort = ((ceilsqrt stat.ev) // 4)
    let score = (((gene + effort) * stat.level) // 100)
    score

struct Stat
    name  : string
    level = 0:u16
    base  = 0:u16
    dv    = 0:u16
    ev    = 0:u16

    let printstat calcpre

    fn calcstat (stat)
        + ('calcpre stat) 5

type StatHP <: Stat
    let printstat calcpre

    fn calcstat (stat)
        + ('calcpre stat) stat.level 10

enum StatCommon
    Stat : Stat
    StatHP : StatHP

fn pokestats (args)
    let level = (@ args 0)
    local hp =
        StatHP
            name = "HP"
            level = level
            base = (@ args 1)
            ev = (@ args 10)
    local atk =
        Stat
            name = "Attack"
            level = level
            base = (@ args 2)
            dv = (@ args 6)
            ev = (@ args 11)
    local def =
        Stat
            name = "Defense"
            level = level
            base = (@ args 3)
            dv = (@ args 7)
            ev = (@ args 12)
    local spd =
        Stat
            name = "Speed"
            level = level
            base = (@ args 4)
            dv = (@ args 8)
            ev = (@ args 13)
    local spc =
        Stat
            name = "Special"
            level = level
            base = (@ args 5)
            dv = (@ args 9)
            ev = (@ args 14)
    hp.dv =
        +
            8:u16 * (atk.dv % 2)
            4:u16 * (def.dv % 2)
            2:u16 * (spd.dv % 2)
            1:u16 * (spc.dv % 2)

    print
        ..
            "Level   : "
            tostring level
    local stats =
        arrayof StatCommon
            StatCommon.StatHP hp
            StatCommon.Stat atk
            StatCommon.Stat def
            StatCommon.Stat spd
            StatCommon.Stat spc
    for stat in stats
        #'apply stat 'printstat
        # 'apply stat
        #     inline (arg)
        #         'printstat (unpack arg)
        dispatch stat
        case Stat (stat)
            'printstat stat
        case StatHP (stat)
            'printstat stat
        default
            unreachable;

fn main ()
    returning Nothing
    let argc argv = (launch-args)
    let n = (argc - 2)
    switch n
    pass 6
    pass 10
    pass 15
    do
        local args = ((array u16 15))
        for i in (range n)
            (@ args i) = ((cstdlib.atoi (@ argv (i + 2))) as u16)
        pokestats args
        exit 0

    default
        # stderr?
        print
            ..
                "Usage: "
                ((@ argv 0) as string)
                " "
                ((@ argv 1) as string)
                " <level> <base> [<dv> [<ev>]]"
        print "For each of base, dv, ev enter the corresponding stats for HP (except on dv), Attack, Defense, Speed, and Special in that order"
        exit 1

main;
