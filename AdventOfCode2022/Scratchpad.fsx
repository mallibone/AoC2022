#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

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

// X means you need to lose, 
// Y means you need to end the round in a draw, and 
// Z means you need to win.
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


// part 1
getInput 2
// getTestInput 2
|> Seq.map splitPlays
|> Seq.sumBy (fun (p1,p2) -> (calculateScore p1 p2) + (calculatePlay p2))


// part 2
getInput 2
// getTestInput 2
|> Seq.map splitPlaysPart2
|> Seq.sumBy (fun (p1,p2) -> (calculateScore p1 p2) + (calculatePlay p2))