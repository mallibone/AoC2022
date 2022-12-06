module Day05

open System.Text.RegularExpressions;
open System

let parseStacks (inputStacks: seq<string>) =
    let revInputStacks = inputStacks |> Seq.rev
    let stackCount = 
        revInputStacks 
        |> Seq.head 
        |> (fun h -> h.Split("  ") |> Seq.map(fun s -> s.Trim() |> int))
        |> Seq.max

    [for i in 0 .. (stackCount - 1) do
        [for inputRow in (revInputStacks |> Seq.skip 1) do
          yield ((inputRow.ToCharArray())[(1 + 4*i)]).ToString()]]
    |> Seq.map (fun r -> r |> Seq.filter (fun e -> e <> " ") |> Seq.rev |> Seq.toList)
    |> Seq.toList

type Move = {Count:int; Source:int; Destination:int}
let parseMoves (inputMoves: seq<string>) =
    inputMoves 
    |> Seq.map
        (fun f -> 
            let regExMatch = Regex.Match(f, "move (\d+) from (\d) to (\d)", RegexOptions.Compiled)
            { Count = (int regExMatch.Groups.[1].Value); 
                Source = (int (regExMatch.Groups.[2].Value)-1); 
                Destination = (int (regExMatch.Groups.[3].Value))-1}
            )
    |> Seq.toList

let parseInput lines =
    let stacks = lines |> Seq.takeWhile(fun l -> l <> "") |> parseStacks

    let movements = lines |> Seq.skipWhile(fun l -> not (l.StartsWith("move"))) |> parseMoves
    stacks, movements

// let makeMoves (stacks:seq<seq<string>>, movements) =
let makeMoves (stacks:list<list<string>>, movements) =
    let makeMove (stacks:list<list<string>>) (movement:Move) =
        let movedPieces = stacks[movement.Source] |> List.take movement.Count |> List.rev
        let source = movement.Source
        let destination = movement.Destination
    
        [for i in 0 .. ((stacks |> Seq.length) - 1) do
            match i with
            | ci when (ci = source) -> yield (stacks[i] |> List.skip movement.Count)
            | ci when (ci = destination) -> yield (movedPieces@stacks[i])
            | _ -> yield stacks[i]
        ]
    movements |> Seq.fold makeMove stacks

let makeMovesPart2 (stacks:list<list<string>>, movements) =
    let makeMove (stacks:list<list<string>>) (movement:Move) =
        let movedPieces = stacks[movement.Source] |> List.take movement.Count
        let source = movement.Source
        let destination = movement.Destination
    
        [for i in 0 .. ((stacks |> Seq.length) - 1) do
            match i with
            | ci when (ci = source) -> yield (stacks[i] |> List.skip movement.Count)
            | ci when (ci = destination) -> yield (movedPieces@stacks[i])
            | _ -> yield stacks[i]
        ]
    movements |> Seq.fold makeMove stacks

let takeHeads stacks =
    stacks |> List.map (fun s -> s |> List.head) |> (fun s -> String.Join("", s))

let part1 input =
    input
    |> parseInput
    |> makeMoves
    |> takeHeads


let part2 input =
    input
    |> parseInput
    |> makeMovesPart2
    |> takeHeads
    
let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %s"

    input
    |> part1
    |> printfn "Part 1: %s"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %s"

    input
    |> part2
    |> printfn "Part 2: %s"

