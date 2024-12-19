module Day19

let parse =
    let colors =
        Seq.head >> String.remove " " >> splitOn ','
        >> Seq.groupBy String.length >> Map

    splitOnEmpty >> fun xs -> colors xs[0], xs[1]

let design p s =
    let matches (s:string) : string seq -> int64 = Seq.filter s.StartsWith >> Seq.length >> int64
    let ks = p |> Map.keys |> List.ofSeq |> List.rev
    
    let rec go =
        memo (function
                | _, "" -> 1L
                | [], _ -> 0L
                | n::xs, s -> let a = go (xs, s)
                              match p[n] |> matches s with
                              | 0L -> a
                              | l -> a + l * go (ks, s.Substring n))

    go (ks, s)

let part1 (p, d) = d |> Seq.filter (design p >> flip (>) 0) |> Seq.length

let part2 (p, d) = d |> Seq.sumBy (design p)

let Solve: string seq -> int * int64 = parse >> both part1 part2
