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

let part1 (r,u) =
    let correct xs =
        if xs |> sort r |> Seq.zip xs |> Seq.forall (uncurry (=)) then mid xs
        else 0
        
    u |> Seq.map correct |> Seq.sum

let part2 (r,u) =
    let incorrect xs =
         let s = xs |> sort r 
         if s |> Seq.zip xs |> Seq.exists (uncurry (<>)) then mid s
         else 0
    
    u |> Seq.map incorrect |> Seq.sum

let Solve: string seq -> int*int = parse >> both part1 part2
