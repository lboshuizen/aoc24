module Day01

let parse =
    let nums = splitOn ' ' >> Seq.filter ((<>) "") >> Seq.map int
    
    Seq.map nums >> Seq.transpose >> Seq.map Seq.sort >> Seq.Tuple2

let part1 (l,r) = l |> Seq.zip r |> Seq.sumBy (fun (l,r) -> (l-r) |> abs)

let part2 (l,r) =
    let cnt = r |> Seq.countBy id |> Map
    
    l |> Seq.filter (flip Map.containsKey cnt) |> Seq.sumBy (fun x -> x * cnt[x]) 

let Solve (xs:string seq) = xs |> parse |> both part1 part2
