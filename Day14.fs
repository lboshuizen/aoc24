module Day14

let inline (|+) f (a, b) = f a, f b

let parse =
    Seq.map (
        parseRegex
            ".?(\d+),(\d+).*?(-?\d+),(-?\d+)"
            (Array.map int
             >> fun a -> (a[0], a[2]), (a[1], a[3]))
    )

let move (mx, my) t (x, y) =
    let norm m p = if p < 0 then m + p else p
    let f w t (p, s) = (p + (t * s)) % w |> norm w

    f mx t x, f my t y

let quads (w, t) xs =
    let q (w, t) =
        [ fun (x, y) -> x < w / 2 && y < t / 2
          fun (x, y) -> x > w / 2 && y < t / 2
          fun (x, y) -> x < w / 2 && y > t / 2
          fun (x, y) -> x > w / 2 && y > t / 2 ]

    q (w, t) |> Seq.map (fun f -> Seq.filter f xs)

let limit = (101, 103)

let part1 =
    Seq.map (move limit 100)
    >> quads limit
    >> Seq.map Seq.length
    >> Seq.product

let part2 _ = None

let Solve: string seq -> int * option<int> = parse >> both part1 part2
