module Day06

let parse: string seq -> Map<int*int,char> = toGrid2d >> Map

let ahead (x,y)   d=
    match d with
    | 0 -> (x,y-1)
    | 1 -> (x+1,y)
    | 2 -> (x,y+1)
    | 3 -> (x-1,y)

let patrol m =
            
    let rec go m d p v =
        let turn = ((d+1) % 4)
        if Set.contains (d,p) v then Seq.empty
        else 
            let n = ahead p d
            match Map.tryFind n m with
            | Some '.' | Some '^'  -> go m d n (Set.add (d,p) v)
            | Some _ -> go m turn p v
            | None -> v |> Set.map snd |> Set.toSeq

    match Map.tryFindKey (fun _ v -> v = '^') m with
    | Some sp -> go m 0 sp Set.empty
    | _ -> Seq.empty
    
let part1 = patrol >> Seq.length >> plus1
  
let part2 m = 
  patrol m
  |> Seq.map (fun p -> patrol (Map.update m p (Const '#')))
  |> Seq.filter ( (=) Seq.empty) |> Seq.length

let Solve: string seq -> int*int = parse >> both part1 part2

