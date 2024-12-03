module Day02

let parse = Seq.map (splitOn ' ' >> Seq.map int)

let isSafe =
    let deltas = Seq.map (fun (a,b) -> b-a )
    let rule (s,t) d = s,t && (sign d = s) && (abs d >= 1 && abs d <= 3) 
    let test xs = xs |> Seq.fold rule (Seq.head xs |> sign,true)
    
    Seq.pairwise >> deltas >> test >> snd

let count = Seq.filter ((=) true) >> Seq.length

let part1 = Seq.map isSafe >> count

let damped xs =
    let remove xs = seq {for i in 1..(Seq.length xs) -> Seq.removeAt (i-1) xs}
    
    if isSafe xs then true
    else
    xs |> remove |> Seq.exists isSafe

let part2 = Seq.map damped >> count

let Solve (xs:string seq) = xs |> parse |> both part1 part2
