module Day13

let parse =
    let nums = parseRegex @".*?(\d+).*?(\d+)" (fun a -> int64 a[0], int64 a[1])

    splitOnEmpty
    >> Seq.map (fun l -> nums l[0], nums l[1], nums l[2])

let eq ((ax: int64, ay: int64), (bx: int64, by: int64), (px: int64, py: int64)) : int64 =
    let n, d = py * bx - px * by, ay * bx - ax * by
    let a = n / d
    let b = px - a * ax

    if n % d = 0 && b % bx = 0 then
        3L * a + b / bx
    else
        0L

let part1 = Seq.map eq >> Seq.sum

let part2 =
    let adjust (a, b, (px, py)) =
        a, b, (px + 10000000000000L, py + 10000000000000L)

    Seq.map adjust >> part1

let Solve: string seq -> int64 * int64 = parse >> both part1 part2
