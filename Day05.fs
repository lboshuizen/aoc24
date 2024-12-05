module Day05

let parse xs =
    let group = Seq.groupBy fst >> Seq.map (fun (k,xs) -> k, (xs |> Seq.map snd) |> Set)
    
    let rules::updates::_ = splitOnEmpty xs
    let r = rules |> Seq.map ( splitOn '|' >> fun a -> (int a[0], int a[1])) |> group |> Map
    let u = updates |> Seq.map (splitOn ',' >> Seq.map int)
    
    (r,u)

let compare (m:Map<int,Set<int>>) a b =
     match m.TryFind a with
     | Some s when s.Contains b -> -1
     | _ -> 1

let sort m = Seq.sortWith (compare m)
let mid xs = xs |> Seq.length |> fun l -> Seq.item (l/2) xs 

let correct r xs =
    if xs |> sort r |> Seq.zip xs |> Seq.forall (uncurry (=)) then Some (mid xs)
    else None

let queue f r = Seq.map (f r) >> Seq.choose id >> Seq.sum

let part1 (r,u) = u |> queue correct r

let incorrect r xs =
     let s = xs |> sort r 
     if s |> Seq.zip xs |> Seq.exists (uncurry (<>)) then Some (mid s)
     else None

let part2 (r,u) = u |> queue incorrect r

let Solve: string seq -> int*int = parse >> both part1 part2
