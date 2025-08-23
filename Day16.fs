(*
Day 16: Reindeer Maze
Pathfinding with directional state: move forward (+1), rotate 90° (+1000)
State space is (position, direction) since turning cost matters
*)

module Day16

open AStar

let inline (++) (a, b) (a', b') = (a + a', b + b')
let point c = Seq.find (snd >> (=) c) >> fst
let parse = toGrid2d >> both Map (point 'S')

let candidates m p =
    [ 0, -1; 0, 1; -1, 0; 1, 0 ]
    |> List.filter (fun d -> Map.containsKey (p ++ d) m && m[p ++ d] <> '#')

let rotateClockwise (dx, dy) = (-dy, dx)
let rotateCounterClockwise (dx, dy) = (dy, -dx)

let dijkstra s e m =
    let mutable queue = [(0, s, (1, 0))]
    let mutable visited = Set.empty
    
    let rec search () =
        match queue with
        | [] -> None
        | (cost, pos, dir) :: rest ->
            queue <- rest
            
            if pos = e then Some cost
            else
                let state = (pos, dir)
                if Set.contains state visited then
                    search ()
                else
                    visited <- Set.add state visited
                    
                    let nextStates = 
                        [
                            let nextPos = pos ++ dir
                            if Map.containsKey nextPos m && m[nextPos] <> '#' then
                                yield (cost + 1, nextPos, dir)
                            
                            yield (cost + 1000, pos, rotateClockwise dir)
                            yield (cost + 1000, pos, rotateCounterClockwise dir)
                        ]
                    
                    for (nextCost, nextPos, nextDir) in nextStates do
                        let nextState = (nextPos, nextDir)
                        if not (Set.contains nextState visited) then
                            let rec insert item lst =
                                match lst with
                                | [] -> [item]
                                | (c, _, _) :: _ as l when nextCost <= c -> item :: l
                                | h :: t -> h :: (insert item t)
                            
                            queue <- insert (nextCost, nextPos, nextDir) queue
                    
                    search ()
    
    search ()

let part1 (m, s) =
    let e = Map.findKey (fun _ v -> v = 'E') m
    dijkstra s e m |> Option.get

// Part 2: Find ALL optimal paths and count unique tiles
type Path = (int * int) list

type SearchNode = {
    Pos: int * int
    Dir: int * int
    Cost: int
    PrevNode: SearchNode option
}

let reconstructPath (node: SearchNode) : Path =
    let rec buildPath node acc =
        match node.PrevNode with
        | None -> node.Pos :: acc
        | Some prev -> buildPath prev (node.Pos :: acc)
    buildPath node []

let allPaths (m: Map<int*int, char>) (s: int*int) (e: int*int) : Path list =
    let optimalCost = dijkstra s e m |> Option.get
    
    let distances = System.Collections.Generic.Dictionary<(int*int)*(int*int), int>()
    let parents = System.Collections.Generic.Dictionary<(int*int)*(int*int), ((int*int)*(int*int)) list>()
    let mutable queue = [(0, s, (1, 0))]
    
    distances.[(s, (1, 0))] <- 0
    parents.[(s, (1, 0))] <- []
    
    while not queue.IsEmpty do
        let (cost, pos, dir) = List.head queue
        queue <- List.tail queue
        
        let currentState = (pos, dir)
        let currentBestCost = if distances.ContainsKey(currentState) then distances.[currentState] else System.Int32.MaxValue
        
        if cost <= currentBestCost then
            let nextStates = 
                [
                    let nextPos = pos ++ dir
                    if Map.containsKey nextPos m && m[nextPos] <> '#' then
                        yield (cost + 1, nextPos, dir)
                    
                    yield (cost + 1000, pos, rotateClockwise dir)
                    yield (cost + 1000, pos, rotateCounterClockwise dir)
                ]
            
            for (nextCost, nextPos, nextDir) in nextStates do
                let nextState = (nextPos, nextDir)
                let nextBestCost = if distances.ContainsKey(nextState) then distances.[nextState] else System.Int32.MaxValue
                
                if nextCost < nextBestCost then
                    distances.[nextState] <- nextCost
                    parents.[nextState] <- [currentState]
                    
                    if nextCost <= optimalCost then
                        let rec insert item lst =
                            match lst with
                            | [] -> [item]
                            | (c, _, _) :: _ as l when nextCost <= c -> item :: l
                            | h :: t -> h :: (insert item t)
                        queue <- insert (nextCost, nextPos, nextDir) queue
                        
                elif nextCost = nextBestCost then
                    let existingParents = if parents.ContainsKey(nextState) then parents.[nextState] else []
                    parents.[nextState] <- currentState :: existingParents
    
    let endStates = 
        distances.Keys 
        |> Seq.filter (fun (pos, _) -> pos = e)
        |> Seq.filter (fun state -> distances.[state] = optimalCost)
        |> List.ofSeq
    
    let rec buildAllPaths state =
        let (pos, _) = state
        let parentStates = if parents.ContainsKey(state) then parents.[state] else []
        
        if List.isEmpty parentStates then
            [[pos]]
        else
            parentStates
            |> List.collect buildAllPaths
            |> List.map (fun path -> pos :: path)
    
    endStates
    |> List.collect buildAllPaths
    |> List.map List.rev

let part2 (m, s) =
    let e = Map.findKey (fun _ v -> v = 'E') m
    let paths = allPaths m s e
    
    paths
    |> List.collect id
    |> Set.ofList
    |> Set.count

let Solve: string seq -> int * int = parse >> both part1 part2
