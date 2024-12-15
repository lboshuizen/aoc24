module Day15

let inline (++) (a, b) (a', b') = (a + a', b + b')

let expand =
    let replace =
        function
        | '#' -> [ '#'; '#' ]
        | '.' -> [ '.'; '.' ]
        | 'O' -> [ '['; ']' ]
        | '@' -> [ '@'; '.' ]

    List.map (Seq.collect replace)

type Grid = Map<int * int, char>

let parse f =
    splitOnEmpty
    >> fun l -> l[0] |> f |> toGrid2d |> Map, l[1] |> Seq.concat

let next =
    function
    | '^' -> 0, -1
    | 'v' -> 0, 1
    | '<' -> -1, 0
    | '>' -> 1, 0

let rec boxes (m: Grid) p d =
    match m[p] with
    | '[' | ']' | 'O' -> p :: boxes m (p ++ d) d
    | '#' -> [ p ]
    | _ -> []

let rec boxesUD (m: Grid) p d =
    match Map.tryFind p m with
    | Some '[' -> p :: (boxesUD (Map.remove p m) (p ++ (1, 0)) d @ boxesUD m (p ++ d) d)
    | Some ']' -> p :: (boxesUD (Map.remove p m) (p ++ (-1, 0)) d @ boxesUD m (p ++ d) d)
    | Some '#' -> [ p ]
    | _ -> []

let boxes2 (m: Grid) p d =
    let orderBy (_, dy) xs = List.sortBy (snd >> fun y -> y * dy) xs

    match d with
    | _, 0 -> boxes m p d
    | _ -> boxesUD m p d |> List.distinct |> orderBy d

let push (m: Grid) d =
    let moveBox (d: int * int) (p: int * int) m =
        let set p v m = Map.update m p (Const v)

        m |> set (p ++ d) m[p] |> set p '.'

    foldr (moveBox d) m

let wall (m: Grid) = Seq.exists (fun p -> m[p] = '#')

let move fb (m: Grid, p) c =
    let d = next c
    let p' = p ++ d

    match m[p'] with
    | '.' -> m, p'
    | '#' -> m, p
    | 'O' | '[' | ']' ->
        let bs = fb m p' d
        if wall m bs then
            m, p
        else
            push m d bs, p'

let start (m: Grid) =
    let s = Map.entries m |> Seq.find (snd >> (=) '@') |> fst
    Map.update m s (Const '.'), s

let gps c = Map.entries >> Seq.filter (fun (_, v) -> v = c) >> Seq.sumBy (fun ((x, y), _) -> x + 100 * y)

let part1 (m, xs) = Seq.fold (move boxes) (start m) xs |> fst |> gps 'O'

let part2 (m, xs) = Seq.fold (move boxes2) (start m) xs |> fst |> gps '['

let Solve: string seq -> int * int = both (parse id >> part1) (parse expand >> part2)
