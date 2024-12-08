module Day08

let antennas =
    Seq.filter (snd >> (<>) '.')
    >> Seq.groupBy snd
    >> Seq.map (snd >> Seq.map fst >> Seq.toList >> combinations)
    >> Seq.concat

let inbounds =
    let clip (mx, my) (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my
    Seq.maxBy fst >> fst >> clip

let parse = toGrid2d >> both antennas inbounds

let antinode n ((x, y), (x', y')) =
    let dx, dy = (n * (x' - x), n * (y' - y))

    [ (x - dx, y - dy); (x' + dx, y' + dy) ]

let count pred =
    Seq.concat
    >> Seq.filter pred
    >> Seq.distinct
    >> Seq.length

let part1 (m, valid) =
    m |> Seq.map (antinode 1) |> count valid

let part2 (m, valid) =

    let lineup n =
        Seq.replicate n >> Seq.mapi antinode >> Seq.concat

    m |> Seq.map (lineup 50) |> count valid

let Solve: string seq -> int * int = parse >> both part1 part2
