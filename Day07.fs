module Day07

open System

let parse = Seq.map( splitOn ':' >> fun a -> int64 a[0], (a[1] |> splitOn ' ' |> Seq.skip 1 |> Seq.map int64 |> Seq.toList))

let concat a b =
     let p = MathF.Log10(float32 b) |> fun l -> pown 10L (int l+1)
     (a * p) + b

let run ops (n,xs)=
   
    let rec go a =
        function
        | x::xs' -> ops |> Seq.tryPick (flip uncurry (a,x) >> flip go xs')
        | [] when a = n -> Some n
        | _ -> None
    
    match go 0L xs with
    | Some v -> v
    | _ -> 0L
    
let part1  = Seq.map (run [(+);(*)]) >> Seq.sum

let part2 = Seq.map (run [(+);(*);concat]) >> Seq.sum

let Solve: string seq -> int64*int64 = parse >> both part1 part2
