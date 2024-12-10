module Day10

let parse =
    toGrid2d
    >> Seq.map (mapSnd a2i)
    >> both Map (Seq.filter (snd >> (=) 0) >> Seq.map fst)

let around p =
    List.map ((++) p) [ (-1, 0); (0, -1); (0, 1); (1, 0) ]

let rec walk (m: Map<int * int, int>) p =
    let candidates h =
        around p
        |> List.filter (fun p' -> Map.containsKey p' m && abs m[p'] - h = 1)

    match m[p] with
    | 9 -> [ p ]
    | h ->
        candidates h
        |> List.collect (walk (Map.remove p m))

let trailhead f m p = walk m p |> f |> List.length

let part1 (m, xs) =
    xs
    |> Seq.map (trailhead List.distinct m)
    |> Seq.sum

let part2 (m, xs) =
    xs |> Seq.map (trailhead id m) |> Seq.sum

let Solve: string seq -> int * int = parse >> both part1 part2
