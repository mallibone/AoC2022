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

type Blueprint = {Id:int;OreRobotOreCost:int;ClayRobotOreCost:int;ObsidianRobotOreCost:int;ObsidianRobotClayCost:int;GeodeRobotOreCost:int;GeodeRobotObsidianCost:int}
let parseInput inputLine =
    inputLine |> function
        // | Match "(.*),(.*),(.*)" [ x; y; z; ] ->
        | Match "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." [ blueprintId; oreRobotCost; clayRobotCost; obsidianRobotOreCost; obsidianRobotClayCost; geodeRobotOreCost; geodeRobotObsidianCost; ] ->
            {Id = int blueprintId; OreRobotOreCost = int oreRobotCost; ClayRobotOreCost = int clayRobotCost; ObsidianRobotOreCost = int obsidianRobotOreCost; ObsidianRobotClayCost = int obsidianRobotClayCost; GeodeRobotObsidianCost = int geodeRobotObsidianCost; GeodeRobotOreCost = int geodeRobotOreCost}

type State = {OreCount:int; ClayCount:int; ObsidianCount:int;GeodeCount:int;OreRobots:int;ClayRobots:int;ObsidianRobots:int;GeodeRobots:int}
let initState = {OreCount=0; ClayCount=0; ObsidianCount=0;GeodeCount=0;OreRobots=1;ClayRobots=0;ObsidianRobots=0;GeodeRobots=0} 

let collectHarvest state =
    let state = 
        {state with 
            OreCount = state.OreCount + state.OreRobots;
            ClayCount = state.ClayCount + state.ClayRobots;
            ObsidianCount = state.ObsidianCount + state.ObsidianRobots;
            GeodeCount = state.GeodeCount + state.GeodeRobots}
    state


let evaluateBluePrint blueprint =
    let rec simMinute state round =
        let state = collectHarvest state
        state

    [0 .. 23] |> Seq.fold simMinute initState
    |> (fun s -> blueprint.Id * s.GeodeCount)

// part 1
// getInput 19
getTestInput 19
|> Array.map parseInput
|> Seq.map evaluateBluePrint
|> Seq.sum

// part 2
// getInput 19
// getTestInput 19
