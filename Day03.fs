module Day03

open System.Text.RegularExpressions

type Ins =
    | Mul of int * int
    | Do
    | Dont

let decode (a: GroupCollection) =
    match a.[0].Value with
    | s when s.StartsWith "mul" -> Mul(a.[1].Value |> int, a.[2].Value |> int)
    | s when s.StartsWith "don" -> Dont
    | s when s.StartsWith "do" -> Do

let exp = @"mul\((?<l>\d+),(?<r>\d+)\)|do.*?\(\)"

let parse =
    Seq.map (fun s ->
        Regex.Matches(s, exp)
        |> Seq.map (fun m -> decode m.Groups))
    >> Seq.concat

let simple =
    function
    | Mul (l, r) -> l * r
    | _ -> 0

let part1 = Seq.sumBy simple

let conditional (a, inc) i =
    match i with
    | Mul (l, r) when inc -> (a + (l * r), inc)
    | Do -> (a, true)
    | Dont -> (a, false)
    | _ -> (a, inc)

let part2 = Seq.fold conditional (0, true) >> fst

let Solve (xs: string seq) = xs |> parse |> both part1 part2
