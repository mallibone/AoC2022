module Day02

type Play =
| Rock
| Paper
| Scissor
| Unkown

let toPlay play =
    match play with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissor
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissor
    | _ -> Unkown

let splitPlays (game:string) =
    let plays = game.Split(" ") |> Array.map toPlay
    (plays[0], plays[1])

let splitPlaysPart2 (game:string) =
    let plays = game.Split(" ") 
    match plays[1] with
    | "X" ->
        match plays[0] with
        | "A" -> Rock, Scissor
        | "B" -> Paper, Rock
        | "C" -> Scissor, Paper
        | _ -> Unkown, Unkown
    | "Z" ->
        match plays[0] with
        | "A" -> Rock, Paper
        | "B" -> Paper, Scissor
        | "C" -> Scissor, Rock
        | _ -> Unkown, Unkown
    | _ ->
        match plays[0] with
        | "A" -> Rock, Rock
        | "B" -> Paper, Paper
        | "C" -> Scissor, Scissor
        | _ -> Unkown, Unkown

let calculateScore p1 p2 =
    match p1, p2 with
    | Rock, Paper -> 6
    | Paper, Scissor -> 6
    | Scissor, Rock -> 6
    | p1, p2 when p1 = p2 -> 3
    | _ -> 0

let calculatePlay p2 =
    match p2 with
    | Rock -> 1
    | Paper -> 2
    | Scissor -> 3
    | _ -> 0

let part1 input =
    input
    |> Seq.map splitPlays
    |> Seq.sumBy (fun (p1,p2) -> (calculateScore p1 p2) + (calculatePlay p2))
    
let part2 input =
    input
    |> Seq.map splitPlaysPart2
    |> Seq.sumBy (fun (p1,p2) -> (calculateScore p1 p2) + (calculatePlay p2))
    
let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %d"

    input
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %d"

    input
    |> part2
    |> printfn "Part 2: %d"

