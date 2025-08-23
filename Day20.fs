/// Advent of Code 2024 - Day 20: Race Condition
/// Find cheats in a racetrack maze that save significant time
module Day20

let directions = [ -1, 0; 1, 0; 0, 1; 0, -1 ]
let around p = directions |> List.map ((++) p)

/// Parse input to extract grid map and start/end positions
let parse input =
    let point c = Seq.find (snd >> (=) c) >> fst
    input |> toGrid2d |> both Map (both (point 'S') (point 'E'))

/// Find the single path from start to end in the maze
let findPath start finish maze =
    let rec walk visited pos =
        match pos with
        | p when p = finish -> pos :: visited
        | _ ->
            around pos
            |> List.find (fun p -> 
                match Map.tryFind p maze with
                | Some '#' | None -> false
                | Some _ -> not (List.contains p visited))
            |> walk (pos :: visited)
    walk [] start |> List.rev

/// Find all valid cheats within maxDist that save at least minSaving time
let findCheats maxDist minSaving path =
    let pathArray = Array.ofList path
    let posIndex = pathArray |> Array.mapi (fun i pos -> pos, i) |> Map.ofArray
    
    // Generate all possible cheat vectors within Manhattan distance
    let deltas = 
        [-maxDist .. maxDist]
        |> List.collect (fun dx ->
            let remaining = maxDist - abs dx
            [-remaining .. remaining]
            |> List.map (fun dy -> dx, dy))
        |> List.filter (function | (0, 0) -> false | _ -> true)
    
    pathArray
    |> Array.mapi (fun i (startX, startY) ->
        deltas
        |> Seq.choose (fun (dx, dy) ->
            match Map.tryFind (startX + dx, startY + dy) posIndex with
            | Some endIdx ->
                let timeSaved = endIdx - i - abs dx - abs dy
                match timeSaved >= minSaving with
                | true -> Some timeSaved
                | false -> None
            | None -> None))
    |> Seq.collect id

/// Solve for a given cheat duration and minimum time saving
let solve maxDist (maze, (start, finish)) =
    findPath start finish maze |> findCheats maxDist 100 |> Seq.length

/// Part 1: 2-picosecond cheats saving ≥100 picoseconds
let part1 = solve 2

/// Part 2: Up to 20-picosecond cheats saving ≥100 picoseconds  
let part2 = solve 20

/// Main solver function
let Solve input = input |> parse |> both part1 part2