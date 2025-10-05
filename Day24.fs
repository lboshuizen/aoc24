/// Day 24: Crossed Wires
/// Boolean gate simulation (Part 1) and binary adder swap detection (Part 2)
module Day24

let parseInitial (line: string) =
    let parts = line.Split(": ")
    (parts[0], int parts[1])

let parseGate (line: string) =
    let parts = line.Split(" ")
    (parts[1], parts[0], parts[2], parts[4])  // (op, in1, in2, out)

let parse input =
    let groups = splitOnEmpty input
    let initial = groups[0] |> List.map parseInitial |> Map.ofList
    let gates = groups[1] |> List.map parseGate
    (initial, gates)

let evalGate op a b =
    match op with
    | "AND" -> a &&& b
    | "OR" -> a ||| b
    | "XOR" -> a ^^^ b
    | _ -> failwith $"Unknown op: {op}"

let simulate (initial, gates) =
    let rec loop wires pending =
        match pending with
        | [] -> wires
        | (op, in1, in2, out) :: rest ->
            match Map.tryFind in1 wires, Map.tryFind in2 wires with
            | Some a, Some b -> loop (Map.add out (evalGate op a b) wires) rest
            | _ -> loop wires (rest @ [(op, in1, in2, out)])
    loop initial gates

let part1 (initial, gates) =
    simulate (initial, gates)
    |> Map.filter (fun (k: string) _ -> k.StartsWith("z"))
    |> Map.toSeq
    |> Seq.sortByDescending fst
    |> Seq.fold (fun acc (_, v) -> acc * 2L + int64 v) 0L

let findGatesByOp gates op = gates |> List.filter (fun (gop, _, _, _) -> gop = op)

let isXY (wire: string) = wire.StartsWith("x") || wire.StartsWith("y")
let isX0Y0 (wire: string) = wire = "x00" || wire = "y00"

let part2 (_, gates) =
    let hasSubgate op (_, _, _, out) = findGatesByOp gates op |> List.exists (fun (_, i1, i2, _) -> i1 = out || i2 = out)

    let isWrong (op, in1, in2, (out: string)) =
        [ out.StartsWith("z") && op <> "XOR" && out <> "z45"
          op = "XOR" && out <> "z00" && not (out.StartsWith("z") || isXY in1 || isXY in2)
          op = "XOR" && isXY in1 && isXY in2 && not (isX0Y0 in1 || isX0Y0 in2) && not (hasSubgate "XOR" (op, in1, in2, out))
          op = "AND" && not (isX0Y0 in1 || isX0Y0 in2) && not (hasSubgate "OR" (op, in1, in2, out)) ]
        |> List.exists id

    gates |> Seq.filter isWrong |> Seq.map (fun (_, _, _, out) -> out) |> Seq.distinct |> Seq.sort |> String.concat ","

let Solve: string seq -> int64 * string = parse >> both part1 part2
