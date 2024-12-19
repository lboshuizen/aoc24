module Day18

open AStar

let inline (|>|) f g a = f a |> g a
let inline (<|>) f g x = (f x, g x)
let inline (++) (a, b) (a', b') = (a + a', b + b')

let parse = Seq.map (splitOn ',' >> fun a -> int a[0], int a[1])

let firstStack n =
    [ for x in 0..n do
          for y in 0..n -> (x, y) ]
    |> Set
    |> Seq.fold (flip Set.remove)

let splitAt n = Seq.take n <|> Seq.skip n

let astar m =
    let around p =
        Seq.map ((++) p) [ -1, 0; 1, 0; 0, 1; 0, -1 ]

    { neighbours = around >> Seq.filter (flip Set.contains m)
      gCost = fun _ _ -> 1
      fCost = fun p p' -> manhattan p p'
      maxIterations = Some 99999 }

let path d = astar >> search (0, 0) (d, d)

let blocks d m =
    Seq.scan (flip Set.remove) m >> Seq.skip 1
    |>| Seq.zip
    >> Seq.find (snd >> path d >> Option.isNone)
    >> fst

let part1 =
    let length = Option.get >> Seq.length >> minus1

    Seq.take 1024
    >> firstStack 70
    >> path 70
    >> length

let part2 =
    splitAt 1024
    >> mapFst (firstStack 70)
    >> uncurry (blocks 70)

let Solve: string seq -> int * (int * int) = parse >> (part1 <|> part2)
