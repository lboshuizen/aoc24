module Day09

type Fat =
    | File of int * int
    | Free of int

let filesystem =
    Seq.chunkBySize 2
    >> Seq.mapi (fun i a -> [ File(i, a.[0]); Free a.[1] ])
    >> Seq.concat
    >> Seq.toList
    >> both
        id
        (List.filter (function
            | File _ -> true
            | _ -> false)
         >> List.rev)

let parse =
    Seq.map a2i
    >> flip Seq.append (Seq.singleton 0)
    >> filesystem

let fit s =
    List.tryFind (function
        | File (_, n) when n <= s -> true
        | _ -> false)

let free fid n =
    List.replace
        (function
        | File (i, _) when fid = i -> true
        | _ -> false)
        (Free n)

let remove i =
    List.filter (function
        | File (n, _) when n = i -> false
        | _ -> true)

let checksum =
    let blocks a =
        function
        | File (i, b) -> a @ List.replicate b i
        | Free n -> a @ List.replicate n 0

    Seq.fold blocks []
    >> Seq.mapi (fun i n -> i * n |> int64)
    >> Seq.sum

let part1 =

    let compact (fs, todo) =

        let rec go fs td a =
            match fs, td with
            | File (i, n) :: fat', File (i2, n2) :: tds when i <> i2 ->
                go fat' (File(i2, n2) :: remove i tds) (File(i, n) :: a)
            | Free x :: fat', File (i, n) :: files' when n <= x -> go (Free(x - n) :: fat') files' (File(i, n) :: a)
            | Free x :: fat', File (i, n) :: files' -> go fat' (File(i, n - x) :: files') (File(i, x) :: a)
            | _ -> List.rev (td @ a)

        go fs todo []

    compact >> checksum

let part2 =
    let compact (fs, todo) =

        let rec go fs td a =
            match fs with
            | File (i, n) :: xs -> go xs (remove i td) (File(i, n) :: a)
            | Free 0 :: xs -> go xs td a
            | Free x :: xs ->
                match fit x td with
                | None -> go xs td (Free x :: a)
                | Some (File (i, n)) -> go (Free(x - n) :: free i n xs) (remove i td) (File(i, n) :: a)
            | _ -> List.rev a

        go fs todo []

    compact >> checksum

let Solve: string seq -> int64 * int64 = Seq.head >> parse >> both part1 part2
