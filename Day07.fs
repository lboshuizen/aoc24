module Day07

let parse = Seq.map( splitOn ':' >> fun a -> int64 a[0], (a[1] |> splitOn ' ' |> Seq.skip 1 |> Seq.map int64 |> Seq.toList))

let eval a b=
    function
    | '+' -> a + b
    | '*' -> a * b
    | '|' -> $"%d{a}%d{b}" |> int64

let run ops (n,xs)=
    
    let rec go xs a =
        match xs with
        | x::xs' -> ops |> Seq.map ((eval a x) >> go xs') |> Seq.choose id |> Seq.tryHead
        | [] when a = n -> Some n
        | _ -> None
    
    match go xs 0L with
    | Some v -> v
    | _ -> 0L
 
let part1  = Seq.map (run ['+';'*']) >> Seq.sum

let part2 = Seq.map (run ['+';'*'; '|']) >> Seq.sum

let Solve: string seq -> int64*int64 = parse >> both part1 part2
