module Day19

open Common
open System.Text.RegularExpressions
open System.Collections.Concurrent
open FSharp.Collections.ParallelSeq

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Blueprint = {Id:int;OreRobotOreCost:int;ClayRobotOreCost:int;ObsidianRobotOreCost:int;ObsidianRobotClayCost:int;GeodeRobotOreCost:int;GeodeRobotObsidianCost:int}
let parseInput inputLine =
    inputLine |> function
        | Match "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." [ blueprintId; oreRobotCost; clayRobotCost; obsidianRobotOreCost; obsidianRobotClayCost; geodeRobotOreCost; geodeRobotObsidianCost; ] ->
            {Id = int blueprintId; OreRobotOreCost = int oreRobotCost; ClayRobotOreCost = int clayRobotCost; ObsidianRobotOreCost = int obsidianRobotOreCost; ObsidianRobotClayCost = int obsidianRobotClayCost; GeodeRobotObsidianCost = int geodeRobotObsidianCost; GeodeRobotOreCost = int geodeRobotOreCost}

type State = {TimeLeft:int; OreCount:int; ClayCount:int; ObsidianCount:int;GeodeCount:int;OreRobots:int;ClayRobots:int;ObsidianRobots:int;GeodeRobots:int}
let initState timeLeft = {TimeLeft=timeLeft; OreCount=0; ClayCount=0; ObsidianCount=0;GeodeCount=0;OreRobots=1;ClayRobots=0;ObsidianRobots=0;GeodeRobots=0} 

let collectHarvest state =
    let state = 
        {state with 
            OreCount = state.OreCount + state.OreRobots;
            ClayCount = state.ClayCount + state.ClayRobots;
            ObsidianCount = state.ObsidianCount + state.ObsidianRobots;
            GeodeCount = state.GeodeCount + state.GeodeRobots}
    state

let passTime state =
    let state = 
        {state with TimeLeft = state.TimeLeft - 1}
    state

let cache = ConcurrentDictionary<Blueprint*State,int>()
let bestPath = ConcurrentDictionary<int,int>()

let rec harvestUntil hasReachedUntil state =
    if hasReachedUntil state || state.TimeLeft = 0 then
        state
    else
        harvestUntil hasReachedUntil ((collectHarvest>>passTime) state)

let createGeoRobot blueprint state =
    {state with GeodeRobots = state.GeodeRobots + 1; OreCount = state.OreCount - blueprint.GeodeRobotOreCost; ObsidianCount = state.ObsidianCount - blueprint.GeodeRobotObsidianCost }

let createObsidianRobot blueprint state =
    {state with ObsidianRobots = state.ObsidianRobots + 1; OreCount = state.OreCount - blueprint.ObsidianRobotOreCost; ClayCount = state.ClayCount - blueprint.ObsidianRobotClayCost }

let createClayRobot blueprint state =
    {state with ClayRobots = state.ClayRobots + 1; OreCount = state.OreCount - blueprint.ClayRobotOreCost }

let createOreRobot blueprint state =
    {state with OreRobots = state.OreRobots + 1; OreCount = state.OreCount - blueprint.OreRobotOreCost }

let maxPotentialOre blueprint = [blueprint.GeodeRobotOreCost;blueprint.ObsidianRobotClayCost;blueprint.ClayRobotOreCost;blueprint.OreRobotOreCost] |> Seq.max
let maxPotentialGeode state = (state.TimeLeft - 1) * state.TimeLeft/2

let discardExtraResources blueprint state = //(time, oreR, clayR, obsR, geoR, ore, clay, obs, geo) =
    let newOreCount = min state.OreCount (state.TimeLeft * (maxPotentialOre blueprint) - state.OreRobots * (state.TimeLeft - 1))
    let newClayCount = min state.ClayCount (state.TimeLeft * blueprint.ObsidianRobotClayCost - state.ClayRobots * (state.TimeLeft - 1))
    let newObisidianCount = min state.ObsidianCount (state.TimeLeft * blueprint.GeodeRobotObsidianCost - state.ObsidianRobots * (state.TimeLeft - 1))
    { state with OreCount = newOreCount; ClayCount = newClayCount; ObsidianCount = newObisidianCount }
    // (time, oreR, clayR, obsR, geoR, newOre, newClay, newObs, geo)

let getNextSteps blueprint state =
    let discardExtraResources = discardExtraResources blueprint
    [
        if state.OreCount >= blueprint.GeodeRobotOreCost && state.ObsidianCount >= blueprint.GeodeRobotObsidianCost then
            // printfn "making geode robot"
            yield (collectHarvest >> createGeoRobot blueprint >> passTime) state
        elif state.ObsidianRobots > 0 then
            yield harvestUntil (fun s -> s.ObsidianCount >= blueprint.GeodeRobotObsidianCost && s.OreCount >= blueprint.GeodeRobotOreCost) state

        if state.ObsidianRobots < blueprint.GeodeRobotObsidianCost then
            if state.OreCount >= blueprint.ObsidianRobotOreCost 
                && state.ClayCount >= blueprint.ObsidianRobotClayCost then
                yield (collectHarvest >> createObsidianRobot blueprint >> passTime) state
            // elif state.ObsidianRobots < blueprint.GeodeRobotObsidianCost && state.ClayRobots > 0 then
            elif state.ClayRobots > 0 then
                yield harvestUntil (fun s -> s.ClayCount >= blueprint.ObsidianRobotClayCost && s.OreCount >= blueprint.ObsidianRobotOreCost) state
            
            if state.OreCount >= blueprint.ClayRobotOreCost && state.ClayRobots < blueprint.ObsidianRobotClayCost then
                yield (collectHarvest >> createClayRobot blueprint >> passTime) state
            elif state.ClayRobots < blueprint.ObsidianRobotClayCost then
                yield harvestUntil (fun s -> s.OreCount >= blueprint.ClayRobotOreCost) state

        if state.OreRobots < (maxPotentialOre blueprint) then
            if state.OreCount >= blueprint.OreRobotOreCost then
                yield (collectHarvest >> createOreRobot blueprint >> passTime) state
            else
                yield harvestUntil (fun s -> s.OreCount >= blueprint.OreRobotOreCost) state
        
        // yield (collectHarvest >> createClayRobot blueprint >> passTime) state
    ]
    |> List.filter(fun candidate -> 
        candidate.GeodeCount + (candidate.GeodeRobots * candidate.TimeLeft) + (maxPotentialGeode state) > bestPath[blueprint.Id])
    |> List.map discardExtraResources

let rec getMaxGeodes blueprint state =
    if bestPath.ContainsKey(blueprint.Id) |> not then
        bestPath[blueprint.Id] <- 0

    match cache.TryGetValue((blueprint, state)) with
    | true, value -> value
    | false, _ ->
        let value = 
            getNextSteps blueprint state
            |> Seq.map (fun n -> 
                    // printfn "%A" state
                    if n.TimeLeft = 0 then
                        if n.GeodeCount > bestPath[blueprint.Id] then bestPath[blueprint.Id] <- n.GeodeCount
                        n.GeodeCount
                    else getMaxGeodes blueprint n)
            |> fun l -> if Seq.isEmpty l then 0 else l |> Seq.max
        
        // printfn "%d" value
        // System.Console.ReadKey() |> ignore
        cache[(blueprint,state)] <- value
        value

let part1 input =
    input 
    |> Seq.map parseInput
    |> PSeq.map (fun blueprint -> blueprint.Id, getMaxGeodes blueprint (initState 24))
    |> Seq.sumBy (fun (id, maxPotentialGeode) -> id * maxPotentialGeode)

let part2 input =
    input 
    |> Array.truncate 3
    |> Seq.map parseInput
    |> PSeq.map (fun blueprint -> getMaxGeodes blueprint (initState 32))
    |> Seq.toList
    |> Seq.reduce (*)

let executeDay day =
    bestPath.Clear()
    cache.Clear()

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
