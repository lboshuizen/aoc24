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

let variance xs =
    let positions = xs |> Seq.toArray
    let n = float positions.Length
    let avgX = positions |> Array.averageBy (fun (x, _) -> float x)
    let avgY = positions |> Array.averageBy (fun (_, y) -> float y)
    let varX = (positions |> Array.sumBy (fun (x, _) -> (float x - avgX) ** 2.0)) / n
    let varY = (positions |> Array.sumBy (fun (_, y) -> (float y - avgY) ** 2.0)) / n
    varX + varY

let part2 robots =
    let maxTime = 101 * 103

    seq { 0 .. maxTime - 1 }
    |> Seq.map (fun t ->
        let positions = robots |> Seq.map (fun (p, v) -> move limit t (p, v))
        t, variance positions)
    |> Seq.minBy snd
    |> fst
    |> Some

let Solve: string seq -> int * option<int> = parse >> both part1 part2
