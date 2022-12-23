#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Valve = {Name:string;FlowRate:int;Edges:list<string>;Paths:list<string*int>}

let parseValveInput inputLines =
    inputLines
    |> Array.map (function
        | Match "Valve (.*) has flow rate=(.*)\; tunnel[s]? lead[s]? to valve[s]? (.*)" [ valve; flowRate; neighbourValves; ] ->
            {Name = valve; FlowRate = int flowRate; Edges = neighbourValves.Split(", ") |> Seq.toList;Paths=[]})

let rec findPaths  valves map distance edges =
    match edges with
    | [] -> map
    | _ ->
        let newDistances = edges |> Seq.map (fun e -> e,distance) |> Seq.toList
        let newEdges = 
            valves
            |> Seq.filter(fun v -> edges |> Seq.contains v.Name ) 
            |> Seq.collect (fun v -> v.Edges)
            |> Seq.filter(fun e -> not (map |> Seq.map fst |> Seq.contains e))
            |> Seq.distinct
            |> Seq.toList
        findPaths valves (map@newDistances) (distance+1) newEdges


let calculatePaths valves =
    // valves |> Seq.map (fun v -> {v with Trips = getTrips valves v})
    valves |> PSeq.map (fun valve -> {valve with Paths = findPaths valves [valve.Name,0] 1 valve.Edges })


let findValve valves name = valves |> Seq.find(fun v -> v.Name = name)

type ValveOpening = {Name:string;FlowRate:int;OpeningRound:int}
let neighbourScore valves valveOpening name =
    let currentValve = findValve valves name
    currentValve.Edges 
    |> List.filter (fun e -> not (valveOpening |> Seq.map (fun vo -> vo.Name) |> Seq.contains e)) 
    |> List.sumBy (fun vn -> (findValve valves vn).FlowRate)

// find paths to all neighbours
let rec openValves roundsLeft (currentPosition:string) valveOpening valves =
    let findValve = findValve valves
    let neighbourScore = neighbourScore valves valveOpening
    if roundsLeft <= 0 then
        // no more time/rounds left
        valveOpening
    else
        let currentValve = findValve currentPosition

        let possiblePaths =
            currentValve.Paths 
            |> Seq.filter(fun (name,_) -> not (valveOpening |> Seq.map (fun vo -> vo.Name) |> Seq.contains name))
        if possiblePaths |> Seq.isEmpty then
            // All valves are opened
            valveOpening
        else
            let nextValve =
                possiblePaths
                |> Seq.map(fun (name,dist) -> name,(findValve name).FlowRate*(roundsLeft - (dist+1)) + neighbourScore name)
                |> Seq.maxBy snd
            let valveOpeningRound = roundsLeft - (snd (currentValve.Paths |> Seq.find(fun p -> (fst p) = (fst nextValve))) + 1)
            let valveToBeOpened = findValve (fst nextValve)
            let newValveOpening = {Name = valveToBeOpened.Name; FlowRate = valveToBeOpened.FlowRate; OpeningRound = valveOpeningRound}
            openValves valveOpeningRound (fst nextValve) (newValveOpening::valveOpening) valves

// part 1
// getInput 16
getTestInput 16
|> parseValveInput
|> calculatePaths
|> Seq.toList
|> openValves 29 "AA" []
|> List.filter(fun vo -> vo.OpeningRound >= 0)
|> List.rev
|> List.sumBy (fun vo -> vo.FlowRate*vo.OpeningRound)

// part 2
// getInput 16
// getTestInput 16
