/// Day 23: LAN Party
/// Find triangles (Part 1) and maximum clique (Part 2) in network graph
module Day23

let addEdge node neighbor =
    Map.change node (function Some s -> Some (Set.add neighbor s) | None -> Some (Set.singleton neighbor))

let parse input =
    input
    |> Seq.map (splitOn '-' >> Seq.Tuple2)
    |> Seq.fold (fun graph (a, b) -> graph |> addEdge a b |> addEdge b a) Map.empty

let neighbors v graph = Map.find v graph

let part1 graph =
    graph
    |> Map.toSeq
    |> Seq.collect (fun (a, ns) ->
        ns |> Seq.collect (fun b ->
            Set.intersect ns (neighbors b graph)
            |> Seq.map (fun c -> [a; b; c] |> List.sort)))
    |> Seq.filter (List.exists (fun (s: string) -> s.StartsWith("t")))
    |> Seq.distinct
    |> Seq.length

let rec bronKerbosch graph r p x =
    if Set.isEmpty p && Set.isEmpty x then [r]
    else
        let pivot = Set.union p x |> Seq.maxBy (fun v -> Set.intersect (neighbors v graph) p |> Set.count)
        let expand (p', x', cliques) v =
            let n = neighbors v graph
            let newCliques = bronKerbosch graph (Set.add v r) (Set.intersect p' n) (Set.intersect x' n)
            (Set.remove v p', Set.add v x', newCliques @ cliques)

        Set.difference p (neighbors pivot graph)
        |> Seq.fold expand (p, x, [])
        |> fun (_, _, cliques) -> cliques

let formatClique clique = clique |> Set.toList |> List.sort |> String.concat ","

let part2 graph =
    bronKerbosch graph Set.empty (graph |> Map.keys |> Set.ofSeq) Set.empty
    |> List.maxBy Set.count
    |> formatClique

let Solve: string seq -> int * string = parse >> both part1 part2
