module Day25

let encode =
    List.rev >> List.tail
    >> List.rev >> List.tail
    >> List.map (List.ofSeq)
    >> List.transpose
    >> List.map (
        List.map (function
            | '#' -> true
            | _ -> false)
    )

let parse =
    splitOnEmpty
    >> List.partition (Seq.head >> Seq.head >> (=) '#')
    >> fun (l, k) -> List.map encode l, List.map encode k

let fit=
    uncurry Seq.zip 
    >> Seq.collect (uncurry Seq.zip >> Seq.map (uncurry (&&)))
    >> Seq.forall not

let fits l = Seq.filter (curry fit l) >> Seq.length 

let count ks = Seq.sumBy (fun l -> fits l ks)

let part1 (l, k) = count l k

let part2 = Const 0

let Solve: string seq -> int * 'b = parse >> both part1 part2
