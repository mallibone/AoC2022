module Day09
open System

type Direction = Up | Down | Left | Right | Unkown
type Movement = {Direction : Direction}
let parseDirection (input:string) =
    match input with
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | _ -> Unkown

let parseMovments (input:string) =
    let parts = input.Split(" ")
    [for i in [0 .. (int parts[1]) - 1] do yield { Direction = parseDirection parts[0]; }]

let moveHead movement (x,y) =
    match movement.Direction with
    | Up ->  (x,y+1)
    | Down -> (x,y-1)
    | Left -> (x-1,y)
    | Right -> (x+1,y)
    | _ -> (x,y)

let moveTail (xt, yt) (xh, yh) =
    let isNoNeighbour (coord1:int) (coord2:int) =
        Math.Abs((coord1 - coord2)) > 1
    if isNoNeighbour xt xh || isNoNeighbour yt yh then
        let nyt = if yt = yh then yt else if yt > yh then yt-1 else yt+1
        let nxt = if xt = xh then xt else if xt > xh then xt-1 else xt+1
        nxt,nyt
    else
        xt,yt

let moveTails(((tails:list<list<int*int>>), (headPos:int*int))) (i:int) =
    let tailPos = tails[i] |> Seq.head
    let newTail = moveTail tailPos headPos
    let newTails = tails |> List.mapi(fun idx t -> if idx = i then newTail::t else t)
    newTails, newTail

let makeAMove ((tailHistory:list<list<int*int>>)) movement =
    let headPos = tailHistory |> Seq.head |> Seq.head
    let newHeadPos = moveHead movement headPos
    let tail = tailHistory |> List.skip 1

    // iterate over all tails, take the latest
    let newTailHistory = 
        tail 
        |> Seq.mapi (fun i _ -> i) 
        |> Seq.fold moveTails (tail, newHeadPos)
        |> fst

    [newHeadPos]::newTailHistory

let part1 input =
    input 
    |> Seq.collect parseMovments
    |> Seq.fold makeAMove [for i in [0 .. 1] do yield [(0,0)]]
    |> Seq.last
    |> Seq.toArray
    |> Set.ofArray
    |> Seq.length

let part2 input =
    input 
    |> Seq.collect parseMovments
    |> Seq.fold makeAMove [for i in [0 .. 9] do yield [(0,0)]]
    |> Seq.last
    |> Seq.toArray
    |> Set.ofArray
    |> Seq.toList
    |> Seq.length

let executeDay day =
    // part 1
    getTestInputLines day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputLines day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    getTestInputLines day
    |> part2
    |> printfn "Part 2 Test: %d"

    getInputLines day
    |> part2
    |> printfn "Part 2: %d"
