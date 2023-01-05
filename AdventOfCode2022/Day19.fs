module Day19

// Kudos: https://topaz.github.io/paste/#XQAAAQBVEAAAAAAAAAAX4HyCZDlA0DjWF7Uex1t2hvV6PgXFw49xPoZBL5Bc9pv9Uq9YIVSoCRefjgScIA0kaT34tDFJHfFhLMG+2+WZXfoZLGpArJkpqZvRn+Oom/t0vSthYU5si2de6TNuEKz3KiRceB5GpWxcceDHPd+L6QUDcAAIxb2iAvfXF8Gr5/urskvtwqkdNGbJEuDDE5w2L0kpxDcUYEILEIziNtpBNQope0sYgkWx42gIUfGu8Trs9KGD4XY+O6/sFM6vMCkxtkkx0QjMbRuqlR6YSc7iSwZ1XBOtNNp0SxxIUXieya1VKKll3tpIdth9uqpo0ZuWQWv8xlwwUTDkL5BBluqMi2l5doTBaqNfUdmXITbbWJIZAYYhCk+ilm5ERHxKlhcx5xnY3c1FoZW+Rq2Muh9hPfs1ToW4y+nCVNJNNdhdq9fXZZdTNQGote5SRA4zq7B7lc5i0SjSEXPrj2wUviF3Xm5R+AgJHnLHBnqc2F8q8slkJhEvDiINW9bOcrkK3x9HM6at/enLzwZ7Wt/wffn4umfxWqKvektAUqrW2pzv/7FPvpU/SzIst/szBSRrRoCsjfr3eomzgrMoc4fSIZZw5jUVtTMTc4OziMJZTDtdHdxePDmyMCNQPytSX0cPfc3cpnpJpGz8lKXGctyY4JLcCGzadYJvbeOyJ4saw6T+Lh5aQl18OpswjMltsPJkm5Kspal9AEwz4oDQd5QMkKkOtuga1h6sMyvknA0HBjtcHuGW/LlGhyL/kB2Y8iPZMda3UD2rBwacxdaKS4VJRvLzhbDdkzMG/Pjg1LSAsC+ddpiQyfkXsSRaIPk9uHohi7C1srr2+H9cQw/v5Jqx5XjGNMmSLT0NSSGaeW6Nsj7W461qjKS448qyy46/VTPvorJx9jPw0h/IA1pzA2MUp/ot6abG1th2j19B4Er/iOiqVAs+fx3fy8SQlgNvBnXLCISgeC+XmbgTL9BeoYWM/QCe7Xx+y8uD1e3Sfvk2Uu9AQPWaMWagwkBQT9yetiT96eIsJXcSLojH6iNxDc794yTRT+CJ+8yTqnQrT3Q7KSnZgcL8AJNyHcI6OdFNIzyesBP4blcLVOPT6FmqfZg6ZBeVmRTK98q+b5Isdn4bmiY77xvSuPl1W4iZ5R4pFYbPKsNwFOTypC1yFf8BGM80HZd7BrjRx+kWxH16JpIBWhL0yAosOVzTxIki/RgmY7zAoRLRHyCuvcoLcA0p+ZMlPExwCjRYljttnc7fHvCbY1kcNg3JV4kuYhnA4kfznArkbQXX12PWr1rs9a8Ttld+5RxPD5Q7+8oLwOS7Y+/vtoAbP9h7wpVa53wDJSNcbZYqagEmJAeEhMsOaIrBKH/1fDQpPbfA1DmG62Pe3PVIdHG1wmdVyPqFoy2QGd1TDl803tbabQWXbg4OvfBiMAttys+pr64JNkrD7lzBWdT+3vyAqpYbn6xAPkw0/80HtPIEKSE9yRIysdNVfWKn9YvViIm2DsaWwbhtYBIhRTs7B4v44jMpiAx2jKqrZrykLMZkwVx4DekuATQrIY8IpAYrGTc7Jy8ciz0lJjQ1J91/PVSlL7wJzVDIjzW58PAyq54QJbbKpTAXsAGr+NEIrMcrg/mZMGSzksmsUBw33b/P+7vdcQlKeIFAl7F29Gj2m9QgGW1lETV0JGg8/fGfyzhKD1y2a73ZqqoRcvYnyVlMF3S2V+QnjZ+lrFKhhnpYMEZsp0pEC4P1Ufn5GmdBedSq6MT7//lEaXU=

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
