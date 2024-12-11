module Day11

let parse = splitOn ' ' >> Seq.map int64

let digits =
    Seq.unfold (function
        | 0L -> None
        | x -> Some(x % 10L, x / 10L))
    >> Seq.rev

let toint = Seq.fold (fun n d -> n * 10L + d) 0L

let rec blink =
    let go =
        function
        | 0, _ -> 1L
        | n, x ->
            match x with
            | 0L -> blink (n - 1, 1L)
            | b when digits b |> Seq.length |> isEven ->
                digits b
                |> Seq.splitInto 2
                |> Seq.sumBy (toint >> curry blink (n - 1))
            | b -> blink (n - 1, b * 2024L)

    memo go

let part1 = Seq.sumBy (curry blink 25)
let part2 = Seq.sumBy (curry blink 75)

let Solve: string seq -> int64 * int64 = Seq.head >> parse >> both part1 part2
