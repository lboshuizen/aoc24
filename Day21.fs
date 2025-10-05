module Day21

// Chain of robots controlling keypads to type numeric door codes

let numPad =
    [ ('7', (0, 0)); ('8', (1, 0)); ('9', (2, 0)); ('4', (0, 1)); ('5', (1, 1)); ('6', (2, 1))
      ('1', (0, 2)); ('2', (1, 2)); ('3', (2, 2)); ('0', (1, 3)); ('A', (2, 3)) ]
    |> Map.ofList

let dirPad =
    [ ('^', (1, 0)); ('A', (2, 0)); ('<', (0, 1)); ('v', (1, 1)); ('>', (2, 1)) ] |> Map.ofList

let getMoves (x1, y1) (x2, y2) =
    let h = String.replicate (abs (x2 - x1)) (if x2 > x1 then ">" else "<")
    let v = String.replicate (abs (y2 - y1)) (if y2 > y1 then "v" else "^")
    [ h + v; v + h ] |> List.distinct

let crossesGap gap (x1, y1) (x2, y2) (path: string) =
    let mutable x, y = x1, y1
    path |> Seq.exists (fun c ->
        (match c with '^' -> y <- y - 1 | 'v' -> y <- y + 1 | '<' -> x <- x - 1 | '>' -> x <- x + 1 | _ -> ())
        (x, y) = gap)

let buildMoves keypad gap =
    [ for from in Map.keys keypad do
          for to' in Map.keys keypad ->
              let p1, p2 = keypad.[from], keypad.[to']
              (from, to'),
              if p1 = p2 then [ "A" ]
              else getMoves p1 p2 |> List.filter (crossesGap gap p1 p2 >> not) |> List.map (fun p -> p + "A") ]
    |> Map.ofList

let numMoves = buildMoves numPad (0, 3)
let dirMoves = buildMoves dirPad (0, 0)
let cache = System.Collections.Generic.Dictionary<_, _>()

let rec minLength (seq: string) depth =
    match cache.TryGetValue((seq, depth)) with
    | true, r -> r
    | _ ->
        let r =
            if depth = 0 then int64 seq.Length
            else
                ("A" + seq)
                |> Seq.pairwise
                |> Seq.sumBy (fun (f, t) ->
                    dirMoves.[f, t] |> List.map (fun m -> minLength m (depth - 1)) |> List.min)
        cache.[(seq, depth)] <- r
        r

let complexity depth (code: string) =
    let len =
        ("A" + code)
        |> Seq.pairwise
        |> Seq.sumBy (fun (f, t) -> numMoves.[f, t] |> List.map (fun m -> minLength m depth) |> List.min)
    len * int64 (code.Substring(0, 3))

let parse = List.ofSeq
let part1 = List.sumBy (complexity 2)
let part2 = List.sumBy (complexity 25)
let Solve: string seq -> int64 * int64 = parse >> both part1 part2
