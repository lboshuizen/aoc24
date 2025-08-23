module Day04

let parse xs =
    let lines = xs |> Seq.filter (System.String.IsNullOrWhiteSpace >> not) |> Array.ofSeq
    let height = lines.Length
    let width = if height > 0 then lines[0].Length else 0
    let grid = Array2D.create height width ' '
    
    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            grid[y, x] <- lines[y][x]
    
    grid

let directions = [|(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)|]
let xmasChars = [|'X';'M';'A';'S'|]

let inline inBounds (grid: char[,]) x y =
    x >= 0 && y >= 0 && y < Array2D.length1 grid && x < Array2D.length2 grid

let checkWord (grid: char[,]) (x,y) (dx,dy) =
    [0..3] |> List.forall (fun i ->
        let nx, ny = x + dx * i, y + dy * i
        inBounds grid nx ny && grid[ny, nx] = xmasChars[i])

let part1 (grid: char[,]) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid
    
    seq { for y in 0 .. height - 1 do
          for x in 0 .. width - 1 do
          for dir in directions do
              if checkWord grid (x,y) dir then 1 else 0 }
    |> Seq.sum

let part2 (grid: char[,]) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid
    
    seq { for y in 1 .. height - 2 do
          for x in 1 .. width - 2 do
              if grid[y, x] = 'A' then
                  let tl = grid[y-1, x-1]
                  let tr = grid[y-1, x+1] 
                  let bl = grid[y+1, x-1]
                  let br = grid[y+1, x+1]
                  
                  let diag1 = (tl = 'M' && br = 'S') || (tl = 'S' && br = 'M')
                  let diag2 = (tr = 'M' && bl = 'S') || (tr = 'S' && bl = 'M')
                  
                  if diag1 && diag2 then 1 else 0
              else 0 }
    |> Seq.sum |> Some

let Solve xs = xs |> parse |> both part1 part2