module Day22

let parse = Seq.map int

let secret =
    let mixpprune f a = a ^^^ f a &&& 0xFFFFFF

    mixpprune (flip (<<<) 6) >> mixpprune (flip (>>>) 5) >> mixpprune (flip (<<<) 11)

let secrets n = flip (Seq.scan (fun p _ -> secret p)) [ 1..n ]

let sequences =
    let index = Seq.map snd >> Seq.map string >> String.concat ""
    let deltas = Seq.map (flip (%) 10) >> Seq.pairwise >> Seq.map (fun (a, b) -> b, b - a)
    let firstOcc = Seq.map (both (Seq.last >> fst) index) >> Seq.distinctBy snd

    deltas >> Seq.windowed 4 >> firstOcc 

let bananas = Seq.groupBy snd >> Seq.map (snd >> Seq.sumBy fst) >> Seq.max

let part1 = Seq.sumBy (secrets 2000 >> Seq.last >> int64)

let part2 = Seq.collect (secrets 2000 >> sequences) >> bananas

let Solve: string seq -> int64 * int = parse >> both part1 part2
