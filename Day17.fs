module Day17

type Cpu =
    { a: int64
      b: int64
      c: int64
      ip: int
      out: int option }

let parse =
    let register = parseRegex @".?(\d+)" (fun a -> a.[0] |> int64)

    let cpu (xs: int64 list) =
        { a = xs[0]
          b = xs[1]
          c = xs[2]
          ip = 0
          out = None }

    let program =
        Seq.head
        >> splitOn ' '
        >> fun a -> a[1]
        >> splitOn ','
        >> Seq.map int
        >> Seq.toArray

    splitOnEmpty
    >> fun l -> l[0] |> (List.map register >> cpu), l[1] |> program

let comboOp cpu =
    function
    | n when n <= 3 -> int64 n
    | 4 -> cpu.a
    | 5 -> cpu.b
    | 6 -> cpu.c
    | _ -> failwith "oops"

let exe cpu (m: int array) =
    let i, op = m[cpu.ip], m[cpu.ip + 1]
    let incIp cpu = { cpu with ip = cpu.ip + 2 }
    let combo = fun () -> comboOp cpu op
    let dv = fun () -> 
        let power = combo ()
        if power >= 63L || power < 0L then 0L  // Avoid overflow/invalid powers for int64
        else 
            let divisor = pown 2L (int power)
            if divisor = 0L then 0L else cpu.a / divisor

    match i with
    | 0 -> { cpu with a = dv () } |> incIp
    | 1 -> { cpu with b = cpu.b ^^^ int64 op } |> incIp

    | 2 -> { cpu with b = combo () % 8L } |> incIp

    | 3 when cpu.a = 0L -> cpu |> incIp
    | 3 -> { cpu with ip = op }

    | 4 -> { cpu with b = cpu.b ^^^ cpu.c } |> incIp
    | 5 -> { cpu with out = Some(int (combo () % 8L)) } |> incIp

    | 6 -> { cpu with b = dv () } |> incIp

    | 7 -> { cpu with c = dv () } |> incIp

    | _ -> failwithf "invalid i:{%d}" i

let run m cpu =
    if cpu.ip < Array.length m then
        let r = exe { cpu with out = None } m
        Some(r.out, r)
    else
        None

let out =
    Seq.choose id
    >> Seq.map string
    >> String.concat ","

let part1 (r, p) = Seq.unfold (run p) r |> out


// Part 2: Find the lowest positive value for register A that makes the program output itself (quine)
//
// Algorithm: Reverse-engineering approach
// 1. The program processes A in 3-bit chunks (A is divided by 8 each iteration)
// 2. Work backwards through the target sequence, trying all possible 3-bit extensions
// 3. For each candidate A, verify it produces the required output suffix
// 4. Recursively build valid solutions from the end of the sequence to the start
// 5. Return the minimum valid A that produces the complete target sequence
let part2 (_, program) =
    let target = Array.toList program
    
    let runProgram a =
        let cpu = { a = a; b = 0L; c = 0L; ip = 0; out = None }
        Seq.unfold (run program) cpu |> Seq.choose id |> Seq.toList
    
    // Reverse-engineering approach: work backwards through target sequence
    let rec solve reversedTarget currentA =
        match reversedTarget with
        | [] -> [currentA]
        | expectedOutput :: remainingTarget ->
            [0L .. 7L]
            |> List.collect (fun i ->
                let candidateA = currentA * 8L + i
                let actualOutput = runProgram candidateA
                
                if actualOutput.Length > 0 && actualOutput.[0] = expectedOutput then
                    solve remainingTarget candidateA
                else
                    []
            )
    
    match solve (List.rev target) 0L with
    | [] -> None
    | solutions -> 
        solutions
        |> List.filter (fun a -> runProgram a = target)
        |> List.sort
        |> function
            | [] -> None
            | minSolution :: _ -> Some minSolution

let Solve: string seq -> 'a * option<int64> = parse >> both (part1 >> fun s -> s) part2
